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



# Import ------------------------------------------------------------------



# Load imputation outputs
# They are currently a list, so need to be processed in D3 before saving in ADM. Will move this in the future.
# For now, load from the datashare.

load(paste0(Code_directory, "/jac_bootstrapEM_140923.rda"))



# Process outputs ---------------------------------------------------------


# Create one dataframe

a<-outputs[1:4] %>% purrr::reduce(full_join, by=c('ID','IMP','YR'))

b<-outputs[5:8] %>% purrr::reduce(full_join, by=c('ID','IMP','YR'))

output_bootstrapEM<-rbind(a,b)

str(output_bootstrapEM)


# get means and confidence intervals

means <- output_bootstrapEM %>%
  group_by(ID) %>%
  dplyr::summarise(across(starts_with("item"), list(mean=mean, sd=sd)))

save(means, file = paste0(Code_directory, "/bootstrapmeans_140923.rda"))


means<-clean_names(means)

# Change negative means to zero. Later, need to add code to change sd to zero when mean is negative


means<-create_zeroes_post(means)


# Keep only means for now. Later, need to add SDs back into the dataset

means_only<-means %>% 
  select(id, ends_with("mean")) %>% 
  rename_with(~str_remove(.,"_mean"), contains("_mean"))

# Add flag - these holdings all have imp_madeup

means_only$imp_madeup<-1


# Merge back with rest of data --------------------------------------------



# Add back to pre_imputation_reduced dataset (i.e. those holdings we created zeroes for in D3)

load(paste0(Code_directory, "/pre_imputation_reduced_zeroes.rda"))

pre_imputation_zeros_2023<-pre_imputation_reduced_zeroes %>% 
  filter(yr==2023) %>% 
  mutate(imp_madeup=0) %>% 
  select(-yr)
  
post_imputation<-rows_update(pre_imputation_zeros_2023, means_only, by="id", unmatched="ignore")

# Merge back into full pre_imputation_rolled_forward (all items) dataset 

# Load pre_imputation rolled forward dataset

load(paste0(Code_directory, "/pre_imputation_rolled_forward.rda"))

pre_imputation<-pre_imputation_rolled_forward

data_2023<-pre_imputation %>% 
  filter(yr==2023) %>% 
  mutate(imp_madeup=0)

# Merge

post_imputation<-rows_update(data_2023, post_imputation, by="id")


# Check this merge has done the job

rolled_forward_saf_items<-c("item1", "item2879", "item2827", "item2828", "item41")
rolled_forward_ags_items<-c("item171", "item192", "item193", "item194", "item195", "item196", "item197", "item198", "item199", "item1714",
                            "item1715","item1716","item1717","item1718","item1719","item2867","item2862","item177","item178","item179",
                            "item182","item183","item184","item2566","item2567")
imputed_items<-c("item14","item24","item94","item139","item140","item141","item143","item144","item145","item2038","item2039",
                 "item2320","item2321","item2322","item2469","item2470","item2472","item2473","item2474","item2862","item2868",
                 "item27710","item27715","item27720","item27725","item27730","item27735","item27740","item27750",
                 "item27755","item27775","item27780") 


checkafter<-post_imputation%>% 
  select(id, parish, holding, land_data, imptype, madeup, saf_madeup, ags_madeup, imp_madeup, rolled_forward_saf_items, rolled_forward_ags_items, imputed_items)


checkbefore<-data_2023%>% 
  select(id, parish, holding, land_data, imptype, madeup, saf_madeup, ags_madeup, imp_madeup, rolled_forward_saf_items, rolled_forward_ags_items, imputed_items)

post_imputation<-post_imputation %>% 
  select(-id)

# Add back into whole population (i.e. to the complete responses). This is the combined dataset for the extract from 04.09.23, at the end of B2. Not the corrected dataset yet. LN - 07.09.23


whole_population <- read_table_from_db(server=server,
                                         database=database,
                                         schema=schema,
                                         table_name="pre_imputation_currentyear_full")

whole_population <- whole_population %>% 
  mutate(flag_not_rf=0,
         imp_madeup=0,
         madeup=as.integer(madeup),
         ags_madeup=as.integer(ags_madeup),
         saf_madeup=as.integer(saf_madeup))


post_imputation_whole_pop<-rows_update(whole_population, post_imputation, by=c("parish", "holding"))

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
  mutate(tot_own_area=as.numeric(tot_own_area)) %>% 
  select(-totarea) 

post_imputation_final<-gtools::smartbind(post_imputation_whole_pop, not_in_population)


# check final


checkfinal<-post_imputation_final %>% 
  select(parish, holding, land_data, imptype, madeup, saf_madeup, ags_madeup, imp_madeup, rolled_forward_saf_items, rolled_forward_ags_items, imputed_items)


# Save  -------------------------------------------------------------------

post_imputation_final <- post_imputation_final %>% 
  select(-in2023, -combined_data_2023ID, -not_in_populationID, pre_imputation_currentyear_fullID)

post_imputation_final<-ungroup(post_imputation_final) 

# Remove functions which have same name as variables. In future, change this. 
rm(ags_madeup, saf_madeup)

post_imputation_final<-post_imputation_final %>% 
  mutate(ags_madeup=as.numeric(ags_madeup),
         saf_madeup=as.numeric(saf_madeup),
         madeup=as.numeric(madeup))


# Save final post-imputation dataset 
post_imputation_final<-as.data.frame(post_imputation_final)

save(post_imputation_final, file = paste0(Code_directory, "/post_imputation.rda"))

# The save to ADM isn't working - Error: All unnamed arguments must be length 1. Fix this... 06.09.23 - LN. 

write_dataframe_to_db(server=server,
                      database=database,
                      schema=schema,
                      table_name="post_imputation",
                      dataframe=post_imputation_final,
                      append_to_existing = FALSE,
                      batch_size=1000,
                      versioned_table=FALSE)



