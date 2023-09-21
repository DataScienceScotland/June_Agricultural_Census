# This script takes in the outputs from the multiple imputation and creates the full post-imputation dataset.
# Created by Lucy Nevard June 2023. 
# Modified by Lucy Nevard 07.09.23


# Before import -----------------------------------------------------------


rm(list = ls())


library(imputeJAS)
library(RtoSQLServer)
library(dplyr)
library(janitor)
library(stringr)



Code_directory <- ("//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Codeconversion_2023/2023")

server <- "s0196a\\ADM"
database <- "RuralAndEnvironmentalScienceFarmingStatistics"
schema <- "juneagriculturalsurvey2023alpha"

source("Functions/Functions.R")
source("item_numbers.R")

# Load botstrapEM imputation outputs------------------------------------------------------------------


# They are currently a list, so need to be processed in D3 before saving in ADM. Will move this in the future.
# For now, load from the datashare.

load(paste0(Code_directory, "/jac_bootstrapEM_140923.rda"))



# Process outputs ---------------------------------------------------------


# Create one dataframe

a<-outputs[1:4] %>% purrr::reduce(full_join, by=c('ID','IMP','YR'))

b<-outputs[5:8] %>% purrr::reduce(full_join, by=c('ID','IMP','YR'))

output_bootstrapEM<-rbind(a,b)

str(output_bootstrapEM)


rm(outputs)

output_bootstrapEM<-clean_names(output_bootstrapEM)


imputed_holdings<-output_bootstrapEM



# Load chainedEQ outputs --------------------------------------------------

# Add this later - 20.09.23




# Process outputs ---------------------------------------------------------


# Add flag - these holdings all have imp_madeup.

imputed_holdings$imp_madeup<-1


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

# check final


check_zero_rf_full<-post_zero_rf_full %>% 
  select(parish, holding, land_data, imptype, madeup, saf_madeup, ags_madeup, imp_madeup, rolled_forward_saf_items, rolled_forward_ags_items, imputed_items)


nlevels(as.factor(post_zero_rf_full$id))

# Save this dataset


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

output_bootstrapEM<-output_bootstrapEM %>% select(-yr)

post_imputation<-rows_update(post_zero_rf_twenty, output_bootstrapEM, by=c("id","imp"))


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



# Add means and confidence intervals back into full dataset  --------------

# Update with imputed means 

meansonly<-means %>% 
  select(id, all_of(imputed_items))

post_imputation_final<-rows_update(post_zero_rf_full, meansonly, by="id")

sdsonly<-means %>% 
  select(id, ends_with("sd"))

post_imputation_final<-merge(post_imputation_final, sdsonly, by="id")

checksds<-post_imputation_final %>% select(id, ends_with("sd"))

# Save  -------------------------------------------------------------------



# Remove functions which have same name as variables. In future, change this. 
rm(ags_madeup, saf_madeup)

post_imputation_final<-post_imputation_final %>% 
  mutate(ags_madeup=as.numeric(ags_madeup),
         saf_madeup=as.numeric(saf_madeup),
         madeup=as.numeric(madeup))


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




