# This script takes in the outputs from the multiple imputation and creates the full post-imputation dataset.
# Created by Lucy Nevard June 2023. 
# Modified by Lucy Nevard 07.09.23


# Before import -----------------------------------------------------------


rm(list = ls())


library(readxl)
library(tidyverse)
library(stringr)
library(janitor)
library(data.table)
library(skimr)
library(haven)
library(RtoSQLServer)


Code_directory <- ("//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Codeconversion_2023/2023")

server <- "s0196a\\ADM"
database <- "RuralAndEnvironmentalScienceFarmingStatistics"
schema <- "juneagriculturalsurvey2023alpha"

source("Functions/Functions.R")
source("item_numbers.R")


# Load chainedEQ outputs --------------------------------------------------

# Comment this back in if using chainedEQ outputs

load(paste0(Code_directory, "/imputation_outputs_chainedEQ_041023_one.rda"))
load(paste0(Code_directory, "/imputation_outputs_chainedEQ_041023_two.rda"))
load(paste0(Code_directory, "/imputation_outputs_chainedEQ_041023_three.rda"))
load(paste0(Code_directory, "/imputation_outputs_chainedEQ_041023_four.rda"))
load(paste0(Code_directory, "/jac_pigs_chainedEQ_041023.rda"))
#
#
outputsone<-do.call(rbind.data.frame, outputs_one)
outputstwo<-do.call(rbind.data.frame, outputs_two)
outputsthree<-do.call(rbind.data.frame, outputs_three)
outputsfour<-do.call(rbind.data.frame, outputs_four)
#
output_chainedEQ<-rbind(outputsone, outputstwo, outputsthree, outputsfour)

str(output_chainedEQ)
#
#
rm(outputs_one, outputs_two, outputs_three, outputs_four)

output_chainedEQ<-clean_names(output_chainedEQ)

#
imputed_holdings<-output_chainedEQ



outputspigs<-do.call(rbind.data.frame, outputspigs)

outputspigs<-clean_names(outputspigs)

imputed_pigs<-outputspigs

# Process outputs ---------------------------------------------------------


# Add flag - these holdings all have imp_madeup.

imputed_holdings$imp_madeup<-1

imputed_pigs$imp_madeup<-1


# Create full dataset with zeroes and rolled forward prior to multiple imputation --------------------------------------------


# Load pre_imputation_reduced_zeros dataset (i.e. including those holdings we created zeroes for in D3)

load(paste0(Code_directory, "/pre_imputation_reduced_zeroes.rda"))

pre_imputation_zeros_2023<-pre_imputation_reduced_zeroes %>% 
  filter(yr==2023) %>% 
  mutate(imp_madeup=0) %>% 
  select(-yr)


# Load pre_imputation rolled forward dataset

load(paste0(Code_directory, "/pre_imputation_rolled_forward.rda"))

pre_imputation<-pre_imputation_rolled_forward

data_2023<-pre_imputation %>% 
  filter(yr==2023) %>% 
  mutate(imp_madeup=0)

# Merge

post_zero_rf<-rows_update(data_2023, pre_imputation_zeros_2023, by="id")


# Add back into whole population (i.e. to the complete responses). This is the combined dataset for the extract from 19.09.23, at the end of B2. Not the corrected dataset yet. LN - 07.09.23


whole_population <- read_table_from_db(server=server,
                                       database=database,
                                       schema=schema,
                                       table_name="pre_imputation_currentyear_full")

whole_population <- whole_population %>% 
  mutate(flag_not_rf=0,
         imp_madeup=0,
         madeup=as.integer(madeup),
         ags_madeup=as.integer(ags_madeup),
         saf_madeup=as.integer(saf_madeup),
         id=paste0(parish,"_",holding))


post_zero_rf_whole_pop<-rows_update(whole_population, post_zero_rf, by=c("parish", "holding"))

# Add back the holdings not in the population (not in RPS address file)

not_in_population <- read_table_from_db(server=server,
                                        database=database,
                                        schema=schema,
                                        table_name="not_in_population")

# Or from datashare

#load(paste0(Code_directory, "/not_in_population.rda"))

not_in_population <- not_in_population %>% 
  mutate(flag_not_rf=0,
         imp_madeup=0) %>%
  mutate(tot_own_area=as.numeric(tot_own_area),
         id=paste0(parish,"_",holding)) %>% 
  select(-totarea) 

post_zero_rf_full<-gtools::smartbind(post_zero_rf_whole_pop, not_in_population)

rownames(post_zero_rf_full)<-NULL


# Add back the rolled forward for incomplete responses (made in D2)

pre_imputation_rolled_forward_incomplete<- read_table_from_db(server=server,
                                        database=database,
                                        schema=schema,
                                        table_name="pre_imputation_rolled_forward_incomplete")


pre_imputation_rolled_forward_incomplete<-pre_imputation_rolled_forward_incomplete %>% 
  filter(yr==2023) %>% 
  select(-yr)

post_zero_rf_full<-rows_update(post_zero_rf_full, pre_imputation_rolled_forward_incomplete, by="id")

# check final


check_zero_rf_full<-post_zero_rf_full %>% 
  select(parish, holding, land_data, imptype, madeup, saf_madeup, ags_madeup, imp_madeup, rolled_forward_saf_items, rolled_forward_ags_items, imputed_items)


nlevels(as.factor(post_zero_rf_full$id))

# Save this dataset

save(post_zero_rf_full, file = paste0(Code_directory, "/post_zero_rf_full.rda"))

# Save to ADM

write_dataframe_to_db(server=server,
                      database=database,
                      schema=schema,
                      table_name="post_zero_rf_full",
                      dataframe=post_zero_rf_full,
                      append_to_existing = FALSE,
                      batch_size=10000,
                      versioned_table=FALSE)

rm(pre_imputation, pre_imputation_reduced_zeroes, pre_imputation_rolled_forward)
gc()

# Create equivalent dataset - 20 copies of the same dataset with imp 1:20 only with imputed items --------

post_zero_rf_imputed<-post_zero_rf_full%>% 
  select(c(id,land_data, imptype, madeup, saf_madeup, ags_madeup, imp_madeup),all_of(imputed_items))


post_zero_rf_twenty<-data.frame()
  
for (i in 1:20) {
  df_temp<-post_zero_rf_imputed
  df_temp$imp<-i
  post_zero_rf_twenty<-rbind(post_zero_rf_twenty, df_temp)
}

nlevels(as.factor(post_zero_rf_twenty$id))


# Add imputed values into full dataset (twenty imps) ----------------------

imputed_holdings<-imputed_holdings %>% select(-yr)



post_imputation<-rows_update(post_zero_rf_twenty, imputed_holdings, by=c("id","imp"))


checkbefore<-post_zero_rf_twenty %>% 
  select(id, imp, land_data, imptype, madeup, saf_madeup, ags_madeup, imp_madeup, imputed_items)

checkafter<-post_imputation %>% 
  select(id, imp, land_data, imptype, madeup, saf_madeup, ags_madeup, imp_madeup, imputed_items)


nlevels(as.factor(post_imputation$id))

# Create mean values and confidence intervals for imputed items ---------------------------

# Dataset with only imputed items

imputed<-post_imputation %>% 
  select(c("imp","id"),all_of(imputed_items))

# Create means and confidence intervals

means <- imputed %>%
  group_by(id) %>%
  dplyr::summarise(across(starts_with("item"), list(mean = ~ifelse(mean(.)<0,0, mean(.)),
                                                                  sd = ~ifelse(mean(.)<0,0, sd(.)))))

nlevels(as.factor(means$id))

means<-means %>% 
  rename_with(~str_remove(.,"_mean"), contains("_mean"))




# Pigs --------------------------------------------------------------------

# Pigs - create equivalent dataset - 20 copies of the same dataset with imp 1:20 only with imputed items

post_zero_rf_imputed_pigs<-post_zero_rf_full%>% 
  select(c(id,land_data, imptype, madeup, saf_madeup, ags_madeup, imp_madeup),all_of(pig_imputed_items))


post_zero_rf_twenty_pigs<-data.frame()

for (i in 1:20) {
  df_temp<-post_zero_rf_imputed_pigs
  df_temp$imp<-i
  post_zero_rf_twenty_pigs<-rbind(post_zero_rf_twenty_pigs, df_temp)
}

nlevels(as.factor(post_zero_rf_twenty_pigs$id))


# Add imputed values into full dataset (twenty imps) ----------------------

imputed_pigs<-imputed_pigs %>% select(-yr)

post_imputation_pigs<-rows_update(post_zero_rf_twenty_pigs, imputed_pigs, by=c("id","imp"))


checkbefore<-post_zero_rf_twenty_pigs %>% 
  select(id, imp, land_data, imptype, madeup, saf_madeup, ags_madeup, imp_madeup, pig_imputed_items)

checkafter<-post_imputation_pigs %>% 
  select(id, imp, land_data, imptype, madeup, saf_madeup, ags_madeup, imp_madeup, pig_imputed_items)


nlevels(as.factor(post_imputation_pigs$id))

# Create mean values and confidence intervals for imputed items ---------------------------

# Dataset with only imputed items

pigs_imputed<-post_imputation_pigs %>% 
  select(c("imp","id"),all_of(pig_imputed_items))

# Create means and confidence intervals

pigs_means <- pigs_imputed %>%
  group_by(id) %>%
  dplyr::summarise(across(starts_with("item"), list(mean = ~ifelse(mean(.)<0,0, mean(.)),
                                                    sd = ~ifelse(mean(.)<0,0, sd(.)))))

nlevels(as.factor(pigs_means$id))

pigs_means<-pigs_means %>% 
  rename_with(~str_remove(.,"_mean"), contains("_mean"))





# Add means and confidence intervals back into full dataset  --------------

# Update with imputed means 

meansonly<-means %>% 
  select(id, all_of(imputed_items))

post_imputation_final<-rows_update(post_zero_rf_full, meansonly, by="id")

sdsonly<-means %>% 
  select(id, ends_with("sd"))

post_imputation_final<-merge(post_imputation_final, sdsonly, by="id")


pigs_meansonly<-pigs_means %>% 
  select(id, all_of(pig_imputed_items))

post_imputation_final<-rows_update(post_imputation_final, pigs_meansonly, by="id")

pigs_sdsonly<-pigs_means %>% 
  select(id, ends_with("sd"))

post_imputation_finaltest<-merge(post_imputation_final, pigs_sdsonly, by="id")

checksds<-post_imputation_final %>% select(id, ends_with("sd"))

checkbefore<-post_zero_rf_full %>% 
  filter(id=="80_36"|id=="51_19"|id=="284_366")%>% 
  select(id, item139)


checkafter<-post_imputation_final %>% 
  filter(id=="80_36"|id=="51_19"|id=="284_366") %>% 
  select(id, item139, item139_sd)


# Formats and corrections

# Remove functions which have same name as variables. In future, change this. 
rm(ags_madeup, saf_madeup)

post_imputation_final<-post_imputation_final %>% 
  mutate(ags_madeup=as.numeric(ags_madeup),
         saf_madeup=as.numeric(saf_madeup),
         madeup=as.numeric(madeup))


post_imputation_final<-post_imputation_final %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(item12 = sum(item20026+item11, na.rm=TRUE), # total area of holding
         item2= ifelse(sum(item2827,item2828,item2879, na.rm=TRUE)>0, sum(item2827,item2828,item2879, na.rm=TRUE), item2), # seasonally rented in land
         item68 = ifelse(is.na(item68), sum(item52, item53, item55, item56, item2323, item59, item60, item61, item63, item64, item65, item66, na.rm=TRUE), item68), # total vegetables
         item35=ifelse(is.na(item35), item68, item35),
         item37 = item76,
         item27740 = ifelse(land_data=="saf", item84, item27740),
         item84=ifelse(is.na(item84), item27740, item84),
         item85=ifelse(imptype=="full", sum(item27750,item2862, na.rm=TRUE), item85),
         item86=ifelse(imptype=="full", sum(item27755,item2867, na.rm=TRUE), item86),
         item38 = sum(item41,item84,item85,item86, na.rm = TRUE),
         item40 = sum(item14, item15, item16, item18, item17, item20, item3156, item19, 
                      item23, item21, item24, item2320, item27, item28, item2034, item29, item30,
                      item31, item2059, item32, item34, item36, item2469, item2470, item35, item37, item38, na.rm=TRUE),
         item46 = sum(item2321, item2322, item40, na.rm=TRUE),
         item50 = sum(item46, item47, item48, item49, na.rm=TRUE),
         item145=sum(item139, item140, item143, item141, item144, na.rm=TRUE),
         item157 = sum(item146, item147,item148, item149,item150, item151, item27760,item27770, item27765, na.rm=TRUE),
         item170 = sum(item158, item159, item160, item161, item162, item163, item164, item167, item1708, item2038, item2039, na.rm=TRUE),
         item200=sum(item1714, item1715, item1716, item1717, item192, item193, item1718, item194, item195, item1719, item196, item197, item198, item199, na.rm=TRUE),
         item2556 = sum(item2858, item2863, na.rm=TRUE), # no solid floor strawberries from SAF
         item2557 = sum(item2859, item2864, na.rm=TRUE),# no solid floor raspberries from SAF
         item2836 = sum(item2860, item2865, na.rm=TRUE),# no solid floor blueberries from SAF
         item6001 = sum(item2862, item2867, na.rm=TRUE)
         )
         

         
# Create items which don't exist in the current dataset (i.e. not on SAF or Ags now)

post_imputation_final<-post_imputation_final %>% 
  mutate(item2713=NA,
         item2714=NA, 
         item2715=NA, 
         item2716=NA, 
         item2717=NA, 
         item2866=NA, 
         item2708=NA, 
         item2709=NA, 
         item2710=NA, 
         item2711=NA,
         item152=NA, 
         item153=NA, 
         item154=NA, 
         item155=NA, 
         item95=NA, 
         item96=NA, 
         item1712=NA, 
         item1713=NA, 
         item98=NA
         
  ) %>% 
  mutate(across(starts_with('item')& !starts_with("item185") &!starts_with("item186"), as.numeric))

# Save  -------------------------------------------------------------------



# Save final post-imputation dataset 


save(post_imputation_final, file = paste0(Code_directory, "/post_imputation.rda"))

# Save to ADM

write_dataframe_to_db(server=server,
                      database=database,
                      schema=schema,
                      table_name="post_imputation",
                      dataframe=post_imputation_final,
                      append_to_existing = FALSE,
                      batch_size=10000,
                      versioned_table=FALSE)



