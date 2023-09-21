# This script takes in the pre-imputation dataset and rolls forward for items which are rolled forward. 
# This needs to be run every time the pre_imputation dataset is updated (i.e. using new data cuts)
# Created by Lucy Nevard 03.08.23.
# Modified by Lucy Nevard 07.09.23


# Before import -----------------------------------------------------------


rm(list=ls())


library(RtoSQLServer)
library(janitor)
library(stringr)
library(dplyr)


source("Functions/Functions.R")

Code_directory <- ("//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Codeconversion_2023/2023")

server <- "s0196a\\ADM"
database <- "RuralAndEnvironmentalScienceFarmingStatistics"
schema <- "juneagriculturalsurvey2023alpha"


# Import ------------------------------------------------------------------


# Load pre_imputation dataset

load(paste0(Code_directory, "/pre_imputation.rda"))

# pre_imputation is a large dataset and is time-consuming to read in from ADM - read in from the datashare instead if needed. 

# pre_imputation<- read_table_from_db(server=server, 
#                                      database=database, 
#                                      schema=schema, 
#                                      table_name="pre_imputation")




# Rolling forward SAF items ---------------------------------------------------------


# SAF items which are rolled forward. This list may be updated if decided by the Census team. 


rolled_forward_saf_items<-c("item1", "item2879", "item2827", "item2828", "item41")

# Keep only necessary items

saf_items<-pre_imputation %>% 
  select(c(id, yr, madeup, imptype, saf_madeup), all_of(rolled_forward_saf_items))

# Keep only holdings where SAF items are NA and saf_madeup <10 in 2021.

saf_rf<-saf_items %>% 
  saf_filter()

saf_rf<-as.data.frame(saf_rf)

# Order dataframe by year. Should change this to tidyverse in the future. 

saf_rf<- saf_rf[order(as.numeric(as.character(saf_rf$yr))), ]

# Alter 2023's saf_madeup to increase by 1 from 2021 (i.e. if 5 in 2021, it will be 6 in 2023). Should we do this before excluding? e.g. make saf_madeup for 2023 and then exclude when >10

saf_rf<-saf_madeup(saf_rf)

# Roll forward items from the previous year if 2023 is missing

saf_rf<-roll_forward(saf_rf)


# Rolling forward Ags items -----------------------------------------------

# Ags items which are rolled forward. This list may be updated if decided by the Census team. 

rolled_forward_ags_items<-c("item171", "item192", "item193", "item194", "item195", "item196", "item197", "item198", "item199", "item1714",
                            "item1715","item1716","item1717","item1718","item1719","item2867","item2862","item177","item178","item179",
                            "item182","item183","item184","item2566","item2567")

# Keep only necessary items

ags_items<-pre_imputation %>% 
  select(c(id, yr, madeup, imptype, ags_madeup, saf_madeup), all_of(rolled_forward_ags_items))

# Keep only holdings where Ags items are NA and ags_madeup <10 OR saf_madeup <10 in 2021. Note that this was originally ags_madeup <10 - Paul suggested we keep holdings with SAF returned in last 10 years even if census hasn't been returned in that time

ags_rf<-ags_items %>% 
  ags_filter()

ags_rf<-as.data.frame(ags_rf)

# Order dataframe by year. Should change this to tidyverse in the future. 

ags_rf<- ags_rf[order(as.numeric(as.character(ags_rf$yr))), ]

# Alter 2023's ags_madeup to increase by 1 from 2021

ags_rf<-ags_madeup(ags_rf)

# Roll forward items from the previous year if 2023 is missing


ags_rf<-roll_forward(ags_rf)



# Adding zeroes -----------------------------------------------------

# SAF items

# Holdings where saf_madeup >=10

saf_not_rf<-saf_items %>% 
  saf_filter_not_rf() 

saf_not_rf<-as.data.frame(saf_not_rf)

saf_not_rf<- saf_not_rf[order(as.numeric(as.character(saf_not_rf$yr))), ]


# Add madeup flag 

saf_not_rf<-saf_madeup(saf_not_rf)

# Produce zeroes when saf_madeup>=10. Also creates flag for "not_rf" to distinguish from other madeup holdings above which have been rolled forward. 

saf_not_rf<-saf_not_rf %>% 
  zero_2023()

# Ags items

# Holdings where ags_madeup >=10 AND saf_madeup>=10

ags_not_rf<-ags_items %>% 
  ags_filter_not_rf()

ags_not_rf<-as.data.frame(ags_not_rf)

ags_not_rf<- ags_not_rf[order(as.numeric(as.character(ags_not_rf$yr))), ]


# Add madeup flag 

ags_not_rf<-ags_madeup(ags_not_rf)

# Produce zeroes when ags_madeup>=10 and saf_madeup>=10. Also creates flag for "not_rf" to distinguish from other madeup holdings above which have been rolled forward. 

ags_not_rf<-ags_not_rf %>% 
  zero_2023()


# Remake dataset ----------------------------------------------------------


# Create flag in SAF rolled forward

saf_rf <- saf_rf %>% 
  mutate(
    flag_not_rf=0
  )

# All holdings with SAF flags 

saf<-rbind(saf_not_rf,saf_rf)

saf_added_2023<-saf %>% 
  filter(yr==2023)

# Change remaining NAs to zero 

saf_added_2023<-saf_added_2023 %>% 
  zero_nas()



# Create flag in Ags rolled forward

ags_rf <- ags_rf %>% 
  mutate(
    flag_not_rf=0
  )

# All holdings with Ags flags

ags<-rbind(ags_not_rf,ags_rf)

ags_added_2023<-ags %>% 
  filter(yr==2023)

# Change remaining NAs to zero 

ags_added_2023<-ags_added_2023 %>% 
  zero_nas()



# Final dataset -----------------------------------------------------------



# Update pre_imputation dataframe with adjusted (rolled forward and zeroed) data

pre_imputation$flag_not_rf<-0

pre_imputation<-rows_update(pre_imputation,saf_added_2023, by=c("id", "yr"))

pre_imputation<-rows_update(pre_imputation,ags_added_2023, by=c("id", "yr"))

itemstest<-pre_imputation %>% 
  select(c(id, yr, madeup, imptype, ags_madeup, saf_madeup), all_of(rolled_forward_ags_items), all_of(rolled_forward_saf_items), flag_not_rf)

# Mutate madeup variable to be max of ags_madeup and saf_madeup

pre_imputation_rolled_forward<-madeup(pre_imputation)

# Check this all worked

items_check<-pre_imputation_rolled_forward %>% 
  select(c(id, yr, madeup, imptype, ags_madeup, saf_madeup), all_of(rolled_forward_ags_items), all_of(rolled_forward_saf_items), flag_not_rf)



# Save --------------------------------------------------------------------



write_dataframe_to_db(server=server,
                      database=database,
                      schema=schema,
                      table_name="pre_imputation_rolled_forward",
                      dataframe=pre_imputation_rolled_forward,
                      append_to_existing = FALSE,
                      batch_size=10000,
                      versioned_table=FALSE)

save(pre_imputation_rolled_forward, file = paste0(Code_directory, "/pre_imputation_rolled_forward.rda"))
