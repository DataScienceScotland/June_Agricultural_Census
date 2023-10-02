# This script takes in the outputs from the multiple imputation and creates the full post-imputation dataset.
# Created by Lucy Nevard June 2023. 
# Modified by Lucy Nevard 07.09.23


# Before import -----------------------------------------------------------


rm(list = ls())


library(imputeJAS)
library(RtoSQLServer)
library(janitor)
library(stringr)
library(dplyr)



Code_directory <- ("//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Codeconversion_2023/2023")

server <- "s0196a\\ADM"
database <- "RuralAndEnvironmentalScienceFarmingStatistics"
schema <- "juneagriculturalsurvey2023alpha"

source("Functions/Functions.R")
source("item_numbers.R")

# Load bootstrapEM imputation outputs------------------------------------------------------------------


# They are currently a list, so need to be processed in D3 before saving in ADM. Will move this in the future.
# For now, load from the datashare.

load(paste0(Code_directory, "/jac_bootstrapEM_260923.rda"))

load(paste0(Code_directory, "/jac_pigs_bootstrapEM_270923.rda"))

# Process outputs ---------------------------------------------------------


# Create one dataframe

a<-outputs[1:4] %>% purrr::reduce(full_join, by=c('ID','IMP','YR'))

b<-outputs[5:8] %>% purrr::reduce(full_join, by=c('ID','IMP','YR'))

output_bootstrapEM<-rbind(a,b)

str(output_bootstrapEM)


rm(outputs)

output_bootstrapEM<-clean_names(output_bootstrapEM)


imputed_holdings<-output_bootstrapEM

# Pigs

outputspigs<-clean_names(outputspigs)

imputed_pigs<-outputspigs

# Load chainedEQ outputs --------------------------------------------------

# Comment this back in if using chainedEQ outputs

# load(paste0(Code_directory, "/imputation_outputs_chainedEQ_200923_one.rda"))
# load(paste0(Code_directory, "/imputation_outputs_chainedEQ_200923_two.rda"))
# load(paste0(Code_directory, "/imputation_outputs_chainedEQ_200923_three.rda"))
# load(paste0(Code_directory, "/imputation_outputs_chainedEQ_200923_four.rda"))
# 
# 
# 
# outputsone<-do.call(rbind.data.frame, outputs_one)
# outputstwo<-do.call(rbind.data.frame, outputs_two)
# outputsthree<-do.call(rbind.data.frame, outputs_three)
# outputsfour<-do.call(rbind.data.frame, outputs_four)
# 
# output_chainedEQ<-rbind(outputsone, outputstwo, outputsthree, outputsfour)
# 
# str(output_chainedEQ)
# 
# 
# rm(outputs_one, outputs_two, outputs_three, outputs_four)
# 
# output_chainedEQ<-clean_names(output_chainedEQ)
# 
# 
# imputed_holdings<-output_chainedEQ
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
  select(id, item146)


checkafter<-post_imputation_final %>% 
  filter(id=="80_36"|id=="51_19"|id=="284_366") %>% 
  select(id, item146, item146_sd)


# Formats and corrections

# Remove functions which have same name as variables. In future, change this. 
rm(ags_madeup, saf_madeup)

post_imputation_final<-post_imputation_final %>% 
  mutate(ags_madeup=as.numeric(ags_madeup),
         saf_madeup=as.numeric(saf_madeup),
         madeup=as.numeric(madeup))

post_imputation_final<-post_imputation_final %>% 
  rowwise() %>% 
  mutate(item12 = sum(item20026,item11, na.rm=TRUE), # total area of holding
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
         

         


# Rounding of livestock

post_imputation_final<-post_imputation_final %>% 
  mutate(across(any_of(c(all_sheep, all_pig, all_poultry, all_other_livestock)), round_half_up, 0))
  

check_rounding<-post_imputation_final %>% 
  select(parish, holding, imptype, land_data, any_of(c(all_sheep, all_pig, all_poultry, all_other_livestock)))




# Disaggregation ----------------------------------------------------------


# Disaggregate summary items on non-SAF returns by the proportions in their last return



land_items<-c("item27710", "item27715", "item27720", "item27725", "item27730", "item27735", "item27740", "item27750", "item27755", 
              "item16", "item18", "item17", "item20", "item19", "item23", 
              "item29", "item30", "item31", "item2059", "item32", "item34",
              "item52", "item53", "item55", "item56", "item2323", "item59", "item60", "item61", "item63", "item64", "item65", "item66",
              "item36", "item70", "item71", "item72", "item2832", "item75", 
              "item2324", "item1709", "item80", "item81", "item1710", "item82", "item83",
              "item2713", "item2858", "item2859", "item2860", "item2861", "item2714", "item2715", "item2716", "item2717", 
              "item2707", "item2863", "item2864", "item2865", "item2866", "item2708", "item2709", "item2710", "item2711")

livestock_items<-c("item27760", "item27775", "item27780", "item152", "item153", "item154", "item155", "item95", "item96", "item1712", "item1713", "item98")


# Load previous years

# previous_years <- read_table_from_db(server=server,
#                                      database=database,
#                                      schema=schema,
#                                      table_name="jac_previous_data_ten_years")


load(paste0(Code_directory, "/previous_years.rda"))

previous_years <- previous_years %>% 
  mutate(yr=year)
# Check glasshouse


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
    
  )

land<-post_imputation_final %>% 
  filter(!(land_data=="saf" | land_data =="both")) %>% 
  select(id, land_data, imptype, yr, madeup, saf_madeup, ags_madeup, land_items)


# Combine datasets

previous_years_land<-previous_years %>% 
  select(any_of(names(land)))

previous_years_land<-subset(previous_years_land, id %in% land$id)

land_full<-bind_rows(land,previous_years_land)

land_full<-land_full %>% 
  filter(!is.na(yr))


# keep only holdings where saf_madeup<10 or ags_madeup<10 in 2021

land_full<-land_full %>% 
  group_by (id) %>% 
  filter((any(saf_madeup<10 & yr==2021 |ags_madeup<10 & yr==2021)))


# Overwrite with the last returned data for each holding


land_full<- land_full[order(as.numeric(as.character(land_full$yr))), ]

land_full<- land_full %>% 
    group_by (id) %>% 
    dplyr::mutate(across(c(item16, item18, item17, item20, item19, item23, 
                                item29,item30,item31,item2059,item32,item34,
                                item52,item53,item55,item56,item2323,item59,item60,item61,item63,item64,item65,item66,
                                item36,item70,item71,item72,item2832,item75, 
                                item2324,item1709,item80,item81,item1710,item82,item83,
                                item2713, item2858, item2859, item2860, item2861, item2714, item2715, item2716, item2717, 
                                item2707, item2863, item2864, item2865, item2866, item2708, item2709, item2710, item2711), 
                         ~ifelse(is.na(.[yr==2023]) & yr==2023, zoo::na.locf(., na.rm = FALSE), .)))

# Keep 2023 data

land_2023<-land_full %>% 
  filter(yr==2023) %>% 
  ungroup()

# Create totals for last returned

land_final<-land_2023 %>% 
  mutate(
         barley_27710=sum(item16,item18, na.rm=TRUE),
         oats_27715=sum(item17,item20, na.rm=TRUE),
         osr_27720=sum(item19,item23, na.rm=TRUE),
         stockfeed_27725=sum(item29,item30,item31,item2059,item32,item34,na.rm=TRUE),
         veginopen_27730=sum(item52,item53,item55,item56,item2323,item59,item60,item61,item63,item64,item65,item66, na.rm=TRUE),
         fruitinopen_27735=sum(item36,item70,item71,item72,item2832,item75, na.rm=TRUE),
         nursery_27740=sum(item2324,item1709,item80,item81,item1710,item82,item83, na.rm=TRUE),
         opensoil_27750=sum(item2713, item2858, item2859, item2860, item2861, item2714, item2715, item2716, item2717, na.rm=TRUE),
         solidfloor_27755=sum(item2707, item2863, item2864, item2865, item2866, item2708, item2709, item2710, item2711, na.rm = TRUE)) %>% 
  mutate(
          prop_16=item16/barley_27710,
          prop_18=item18/barley_27710,
          prop_17=item17/oats_27715,
          prop_20=item20/oats_27715,
          prop_19=item19/osr_27720,
          prop_23=item23/osr_27720,
          prop_29=item29/stockfeed_27725,
          prop_30=item30/stockfeed_27725,
          prop_31=item31/stockfeed_27725,
          prop_2059=item2059/stockfeed_27725,
          prop_32=item32/stockfeed_27725,
          prop_34=item34/stockfeed_27725,
          prop_52=item52/veginopen_27730,
          prop_53=item53/veginopen_27730,
          prop_55=item55/veginopen_27730,
          prop_56=item56/veginopen_27730,
          prop_2323=item2323/veginopen_27730,
          prop_59=item59/veginopen_27730,
          prop_60=item60/veginopen_27730,
          prop_61=item61/veginopen_27730,
          prop_63=item63/veginopen_27730,
          prop_64=item64/veginopen_27730,
          prop_65=item65/veginopen_27730,
          prop_66=item66/veginopen_27730,
          prop_36=item36/fruitinopen_27735,
          prop_70=item70/fruitinopen_27735,
          prop_71=item71/fruitinopen_27735,
          prop_72=item72/fruitinopen_27735,
          prop_2832=item2832/fruitinopen_27735,
          prop_75=item75/fruitinopen_27735,
          prop_2324=item2324/nursery_27740,
          prop_1709=item1709/nursery_27740,
          prop_80=item80/nursery_27740,
          prop_81=item81/nursery_27740,
          prop_1710=item1710/nursery_27740,
          prop_82=item82/nursery_27740,
          prop_83=item83/nursery_27740,
          prop_2713=item2713/opensoil_27750,
          prop_2858=item2858/opensoil_27750,
          prop_2859=item2859/opensoil_27750,
          prop_2860=item2860/opensoil_27750,
          prop_2861=item2861/opensoil_27750,
          prop_2714=item2714/opensoil_27750,
          prop_2715=item2715/opensoil_27750,
          prop_2716=item2716/opensoil_27750,
          prop_2717=item2717/opensoil_27750,
          prop_2707=item2707/solidfloor_27755,
          prop_2863=item2863/solidfloor_27755,
          prop_2864=item2864/solidfloor_27755,
          prop_2865=item2865/solidfloor_27755,
          prop_2866=item2866/solidfloor_27755,
          prop_2708=item2708/solidfloor_27755,
          prop_2709=item2709/solidfloor_27755,
          prop_2710=item2710/solidfloor_27755,
          prop_2711=item2711/solidfloor_27755) %>% 
  mutate_all(function(x) ifelse(is.nan(x), 0, x)) %>% 
  mutate(
    item16=prop_16*item27710,
    item18=prop_18*item27710,
    item17=prop_17*item27715,
    item20=prop_20*item27715,
    item19=prop_19*item27720,
    item23=prop_23*item27720,
    item29=prop_29*item27725,
    item30=prop_30*item27725,
    item31=prop_31*item27725,
    item2059=prop_2059*item27725,
    item32=prop_32*item27725,
    item34=prop_34*item27725,
    item52=prop_52*item27730,
    item53=prop_53*item27730,
    item55=prop_55*item27730,
    item56=prop_56*item27730,
    item2323=prop_2323*item27730,
    item59=prop_59*item27730,
    item60=prop_60*item27730,
    item61=prop_61*item27730,
    item63=prop_63*item27730,
    item64=prop_64*item27730,
    item65=prop_65*item27730,
    item66=prop_66*item27730,
    item36=prop_36*item27735,
    item70=prop_70*item27735,
    item71=prop_71*item27735,
    item72=prop_72*item27735,
    item2832=prop_2832*item27735,
    item75=prop_75*item27735,
    item2324=prop_2324*item27740,
    item1709=prop_1709*item27740,
    item80=prop_80*item27740,
    item81=prop_81*item27740,
    item1710=prop_1710*item27740,
    item82=prop_82*item27740,
    item83=prop_83*item27740,
    item2713=prop_2713*item27750,
    item2858=prop_2858*item27750,
    item2859=prop_2859*item27750,
    item2860=prop_2860*item27750,
    item2861=prop_2861*item27750,
    item2714=prop_2714*item27750,
    item2715=prop_2715*item27750,
    item2716=prop_2716*item27750,
    item2717=prop_2717*item27750,
    item2707=prop_2707*item27755,
    item2863=prop_2863*item27755,
    item2864=prop_2864*item27755,
    item2865=prop_2865*item27755,
    item2866=prop_2866*item27755,
    item2708=prop_2708*item27755,
    item2709=prop_2709*item27755,
    item2710=prop_2710*item27755,
    item2711=prop_2711*item27755
  )



# Livestock items 


livestock<-post_imputation_final %>% 
  select(id, land_data, imptype, yr, madeup, saf_madeup, ags_madeup, any_of(livestock_items))

# Combine datasets

previous_years__livestock<-previous_years %>% 
  select(any_of(names(livestock)))

previous_years_livestock<-subset(previous_years__livestock, id %in% livestock$id)

livestock_full<-bind_rows(livestock, previous_years_livestock)

# keep only holdings where ags_madeup<10 in 2021

livestock_full<-livestock_full %>% 
  group_by (id) %>% 
  filter((any(ags_madeup<10 & yr==2021)))


# overwrite with previous return

livestock_full<- livestock_full[order(as.numeric(as.character(livestock_full$yr))), ]

livestock_full <- livestock_full %>% 
group_by (id) %>% 
  dplyr::mutate(across(c(item152, item153, item154, item155, item95, item96, item1712,item1713,item98), 
                       ~ifelse(is.na(.[yr==2023]) & yr==2023, zoo::na.locf(., na.rm = FALSE), .))) 


# Keep 2023 data

livestock_2023<-livestock_full %>% 
  filter(yr==2023)

# Create totals

livestock_2023<-livestock_full %>% 
  mutate(horses_27775=sum(item95,item96, na.rm=TRUE),
         goats_27780=sum(item1712,item1713,item98, na.rm=TRUE),
         fat_pigs27760=sum(item152, item153, item154, item155)) %>% 
  mutate(prop_95=item95/horses_27775,
         prop_96=item96/horses_27775,
         prop_1712=item1712/goats_27780,
         prop_1713=item1713/goats_27780,
         prop_98=item98/goats_27780,
         prop_152=item152/fat_pigs27760,
         prop_153=item153/fat_pigs27760,
         prop_154=item154/fat_pigs27760,
         prop_155=item155/fat_pigs27760) %>% 
  mutate_all(function(x) ifelse(is.nan(x), 0, x)) %>% 
  mutate(item95=prop_95*item27775,
         item96=prop_96*item27775,
         item1712=prop_1712*item27780,
         item1713=prop_1713*item27780,
         item98=prop_98*item27780,
         item152=prop_152*item27760,
         item153=prop_153*item27760,
         item154=prop_154*item27760,
         item155=prop_155*item27760)
  

# Overwrite post_imputation dataset

post_imputation<-rows_update(post_imputation_final, land_2023, by="id")


post_imputation<-rows_update(post_imputation, livestock_2023, by="id")

post_imputation_final<-post_imputation_final %>% 
  mutate(across(any_of(c(land_items, livestock_items)), round_half_up, 2))

checkbefore<-post_imputation_final %>% 
  select(parish, holding, imptype, land_data, any_of(c(land_items, livestock_items)))

checkafter<-post_imputation_final %>% 
  select(parish, holding, imptype, land_data, any_of(c(land_items, livestock_items)))
  
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



