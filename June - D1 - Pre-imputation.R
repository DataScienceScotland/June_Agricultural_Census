# This script creates the pre_imputation dataset. This combines the data from the current year, the previous ten years historic data, and the census population from the RP&S address file.
# This needs to be run every time the combinef_data or combined_data_2023_corrected is updated (i.e. using new data cuts)
# This script is based on programs D0-D2 in the June SAS project (\\s0177a\datashare\seerad\ags\census\branch1\NewStructure\Surveys\June\Main\JUNE CENSUS PROJECT - 2021 Provisional)
# Created by Lucy Nevard July 2023.
# Modified by Lucy Nevard 25.09.23


# Before import -----------------------------------------------------------

# Clear workspace and load packages

rm(list = ls())


library(imputeJAS)
library(RtoSQLServer)
library(dplyr)
library(janitor)
library(stringr)
library(plyr)
library(mirt)
library(data.table)

# Load functions

# source("Functions/Functions.R")
# source("item_numbers.R")

# Directories

Code_directory <- ("//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Codeconversion_2023/2023")

server <- "s0196a\\ADM"
database <- "RuralAndEnvironmentalScienceFarmingStatistics"
schema <- "juneagriculturalsurvey2023alpha"


# Load and prepare datasets ------------------------------------------------------------------


# Previous years is a large dataset and is time-consuming to read in from ADM - read in from the datashare instead if needed. 

# previous_years <- read_table_from_db(server=server,
#                                      database=database,
#                                      schema=schema,
#                                      table_name="jac_previous_data_ten_years")


load(paste0(Code_directory, "/previous_years.rda"))


previous_years_full<-previous_years

# Load combined_data_2023.

# As of 26/09/23 this is the corrected combined dataset produced at the end of C3 (post validations and corrections). It is from the Ags extract on 26/09/23. 



combined_data_2023 <- read_table_from_db(server=server,
                                database=database,
                                schema=schema,
                                table_name="combined_data_2023_corrected") 


data_2023 <- combined_data_2023 %>% 
  dplyr::mutate(in2023="1") 


# Load population frame


#load(paste0(Code_directory, "/address_occid_01jun_2023.rda"))


df_addressfile<-read_table_from_db(server=server,
                                    database=database,
                                    schema=schema,
                                    table_name="address_occid_01jun_2023")

# Choose holdings with the appropriate classification. 


df_addressfile<-clean_names(df_addressfile)

holding_classification<-c(1,2,4,5,6,7,13)

population<-df_addressfile %>% 
  filter(holding_classification_id %in% holding_classification)

# Keep necessary variables

population<-population %>% 
  select(parish,holding,tot_area,tot_own_area)


# Combine population with current data ------------------------------------


# Combine population with current year's data - this keeps only holdings within the population.

pre_imputation_2023<-left_join(population, data_2023, by=c("parish", "holding"))

# Correct items and variables. Add item11 (total area owned), item12 (total area), item20026 (total area rented) if item12 is missing.
# Mutate variables for those holdings not in the responses dataset
pre_imputation_2023<-pre_imputation_2023 %>% 
  mutate(
    item11=ifelse(is.na(item12), tot_own_area, item11),
    item20026=ifelse(is.na(item12), (tot_area-tot_own_area), item20026),
    item12=ifelse(is.na(item12), tot_area,item12)
  ) %>% 
  mutate(
    survdata=ifelse(is.na(in2023), "none", survdata),
    land_data=ifelse(is.na(in2023), "none", land_data),
    other_data=ifelse(is.na(in2023), "no", other_data),
    imptype=ifelse(is.na(in2023), "full", imptype),
    survtype=ifelse(is.na(in2023), "none", survtype),
    
  )

# Add necessary variables 

pre_imputation_2023$yr<-"2023"

pre_imputation_2023$yr<-as.numeric(pre_imputation_2023$yr)

pre_imputation_2023<-pre_imputation_2023 %>% 
  mutate(
    madeup=NA,
    saf_madeup=NA,
    ags_madeup=NA
  )


# Save the current year's full pre-imputation dataset to datashare and ADM


save(pre_imputation_2023, file = paste0(Code_directory, "/pre_imputation_2023_full.rda"))

write_dataframe_to_db(server=server,
                      database=database,
                      schema=schema,
                      table_name="pre_imputation_currentyear_full",
                      dataframe=pre_imputation_2023,
                      append_to_existing = FALSE,
                      batch_size=10000,
                      versioned_table=FALSE)

# Find holdings not in the population

not_in_population<-anti_join(data_2023,population, by=c("parish", "holding"))

# Add necessary variables 

not_in_population$yr<-"2023"

not_in_population$yr<-as.numeric(not_in_population$yr)

not_in_population<-not_in_population %>% 
  mutate(
    madeup=0,
    saf_madeup=0,
    ags_madeup=0,
    totarea="notinpop",
    tot_own_area="notinpop"
  )

# Save these holdings to be added back after all imputation complete

save(not_in_population, file = paste0(Code_directory, "/not_in_population.rda"))

write_dataframe_to_db(server=server,
                      database=database,
                      schema=schema,
                      table_name="not_in_population",
                      dataframe=not_in_population,
                      append_to_existing = FALSE,
                      batch_size=10000,
                      versioned_table=FALSE)



# Reduce size of current dataset ------------------------------------------


# Remove module items to reduce size of the dataset. Fix this in future to remove all module items
pre_imputation_2023<-pre_imputation_2023 %>% 
  select(-(any_of(section_12))) %>% 
  select(-(any_of(section_13)))

pre_imputation_2023_full<-pre_imputation_2023

# Create main pre-imputation dataset - no response ------------------------


# Keep only holdings which need to be imputed - non-response

pre_imputation_2023<-pre_imputation_2023_full %>% 
  filter(!imptype=="none")


# Combine with historic data ---------------------------------------------


# Create id variable

pre_imputation_2023$id<-paste0(pre_imputation_2023$parish,"_",pre_imputation_2023$holding)

# Keep only historic holdings which are in the current dataset

previous_years<-subset(previous_years, id %in% pre_imputation_2023$id)

#take out text items as they're causing issues with binding

pre_imputation_2023<-pre_imputation_2023 %>% 
  select(-item185, -item186)


# Keep only historic items which are in the current dataset

previous_years<-previous_years %>% 
  select(any_of(names(pre_imputation_2023)))



# Combine the two datasets

pre_imputation<-bind_rows(pre_imputation_2023,previous_years)

# Convert id to a factor

pre_imputation$id<-as.factor(pre_imputation$id)

# Exclude if year is missing

pre_imputation<-pre_imputation %>% 
  filter(!is.na(yr))

# Change all blanks to NA

pre_imputation[pre_imputation==""] <- NA


# Create second pre-imputation dataset for incomplete responses -----------


pre_imputation_2023_incomplete<-combined_data_2023 %>% 
  filter(survdata=="full") %>% 
  filter(item2727==0 & item2980 ==0 | item2727==0 & (item2877+item2878)==0 | (item192+item193+item194+item195+item196+item197+item198+item199+item1714+item1715+
                                                                                item1716+item1717+item1718+item1719)==0)

pre_imputation_2023_incomplete<-pre_imputation_2023_incomplete %>% 
  mutate(yr=2023)

pre_imputation_2023_incomplete<-pre_imputation_2023_incomplete %>% 
  select(-(any_of(section_12))) %>% 
  select(-(any_of(section_13)))


# Create id variable

pre_imputation_2023_incomplete$id<-paste0(pre_imputation_2023_incomplete$parish,"_",pre_imputation_2023_incomplete$holding)


# Keep only historic holdings which are in the current dataset

previous_years_incomplete<-subset(previous_years_full, id %in% pre_imputation_2023_incomplete$id)

#take out text items as they're causing issues with binding

pre_imputation_2023_incomplete<-pre_imputation_2023_incomplete %>% 
  select(-item185, -item186)


# Keep only historic items which are in the current dataset

previous_years_incomplete<-previous_years_incomplete %>% 
  select(id, any_of(names(pre_imputation_2023)))

# Combine the two datasets

pre_imputation_incomplete<-bind_rows(pre_imputation_2023_incomplete,previous_years_incomplete)

# Convert id to a factor

pre_imputation_incomplete$id<-as.factor(pre_imputation_incomplete$id)

# Exclude if year is missing

pre_imputation_incomplete<-pre_imputation_incomplete %>% 
  filter(!is.na(yr))

# Change all blanks to NA

pre_imputation_incomplete[pre_imputation_incomplete==""] <- NA



# Save --------------------------------------------------------------------


# Save to ADM and datashare for use in D2. 
# Main dataset

write_dataframe_to_db(server=server,
                      database=database,
                      schema=schema,
                      table_name="pre_imputation",
                      dataframe=pre_imputation,
                      append_to_existing = FALSE,
                      batch_size=10000,
                      versioned_table=FALSE)

save(pre_imputation, file = paste0(Code_directory, "/pre_imputation.rda"))

# Second dataset

write_dataframe_to_db(server=server,
                      database=database,
                      schema=schema,
                      table_name="pre_imputation_incomplete",
                      dataframe=pre_imputation_incomplete,
                      append_to_existing = FALSE,
                      batch_size=10000,
                      versioned_table=FALSE)

save(pre_imputation_incomplete, file = paste0(Code_directory, "/pre_imputation_incomplete.rda"))
