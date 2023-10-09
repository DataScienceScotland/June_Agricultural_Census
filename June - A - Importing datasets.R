# This script imports the datasets from SAF, Ags (the census returns) from this year,the Crofting Commission, ScotEID and the address file sourced from RP&S. They are saved as rdas in the datashare and tables on the ADM server.
# Update the directories at the top to get data drops from the correct years.
# The data used currently is from June 2023.
# This script is based on programs A1, A2, A2.(a) and A3 in the June SAS project (\\s0177a\datashare\seerad\ags\census\branch1\NewStructure\Surveys\June\Main\JUNE CENSUS PROJECT - 2021 Provisional)
# Created by Lucy Nevard 24.02.23 (original individual scripts created 22.11.22),
# Modified by Lucy Nevard 03.10.23



# Setup before import -----------------------------------------------------------


# Clear environment prior

rm(list = ls())


# Load packages

library(readxl)
library(data.table)
library(dplyr)
library(stringr)
library(tidyverse)
library(RtoSQLServer)
library(janitor)
library(openxlsx)
library(haven)

# Load functions

source("Functions/Functions.R")

# Datashare file paths for import

SAF_directory <- ("//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Main/June23/Data Drops/SAF")
AGS_directory <- ("//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Main/June23/Data Drops/AGS")
Croft_directory <- ("//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Main/June23/Data Drops/Crofting Commission")
ScotEID_directory <- ("//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Main/June23/Data Drops/ScotEID")
SAS_directory <- ("//s0177a/sasdata1/ags/census/agscens")

# Datashare file path for export

Code_directory <- ("//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Codeconversion_2023/2023")

# ADM schema for export

server <- "s0196a\\ADM"
database <- "RuralAndEnvironmentalScienceFarmingStatistics"
schema <- "juneagriculturalsurvey2023alpha"

# schema <- "agriculture"  #for the general schema


# Import data -------------------------------------------------------------
# 
# Import translation table for SAF codes

# newcodetrans <-
#    read.csv(paste0(SAF_directory, "/NEW_CODE_TRANS23.csv"))
# 
# newcodetrans<-newcodetrans %>% 
#    select(-SAF.item)
# 
# 
# write_dataframe_to_db(server=server,
#                       database=database,
#                       schema=schema,
#                       table_name="newcodetrans",
#                       dataframe=newcodetrans,
#                       append_to_existing = FALSE,
#                       versioned_table=FALSE,
#                       batch_size = 10000)
# 
# # Bring in 2015 SAF dataset
# 
# saf2015<-read_sas("//s0177a/sasdata1/ags/census/agstemp/allsaf15.sas7bdat")
# 
# 
# write_dataframe_to_db(server=server,
#                       database=database,
#                       schema=schema,
#                       table_name="saf2015",
#                       dataframe=saf2015,
#                       append_to_existing = FALSE,
#                       versioned_table=FALSE,
#                       batch_size = 10000)
# 
# # Bring in June 2015 census dataset (original robust_new)
# 
# june2015<-read_sas("//s0177a/sasdata1/ags/census/agscens/june15.sas7bdat")
# 
# write_dataframe_to_db(server=server,
#                       database=database,
#                       schema=schema,
#                       table_name="june2015_robust_new",
#                       dataframe=june2015,
#                       append_to_existing = FALSE,
#                       versioned_table=FALSE,
#                       batch_size = 10000)

# # Bring in 2022 SAF dataset
# 
 saf2022<-read_sas("//s0177a/sasdata1/ags/census/agstemp/allsaf22.sas7bdat")
# 
# 
 write_dataframe_to_db(server=server,
                      database=database,
                      schema=schema,
                      table_name="saf2022",
                      dataframe=saf2022,
                      append_to_existing = FALSE,
                      versioned_table=FALSE,
                      batch_size = 10000)

# Import Ags data (downloaded from Ags)


df_nonSAF<-read.csv(paste0(AGS_directory,"/June_extract_031023.csv"),
                    fileEncoding="latin1")  # There is no SAF/non-SAF from 2023 onwards. All forms are essentially equivalent to non-SAF.

# # # Tidy Ags data ----------------------------------------------------------------
# 

# Clean names

df_nonSAF <- subset(df_nonSAF, select = -c(item21310, X))



# # Check variables types are correct
#
str(df_nonSAF, list.len = ncol(df_nonSAF))
#
# # Change variable types as needed
#
df_nonSAF <- df_nonSAF %>%
   mutate(
      item185 = as.character(item185),
      item186 = as.character(item186)
   )


# Save rda


save(df_nonSAF, file = paste0(Code_directory, "/Ags_A_2023.rda"))

# Save to ADM

write_dataframe_to_db(server=server,
                      database=database,
                      schema=schema,
                      table_name="Ags_A_2023",
                      dataframe=df_nonSAF,
                      append_to_existing = FALSE,
                      versioned_table=FALSE,
                      batch_size = 10000)


# Import SAF data ---------------------------------------------------------


# # Import SAF datasets separately. Filename will be different for the data drops in June and September.
# #
df_permanent <- read.xlsx(paste0(SAF_directory, "/permanent_output_aug_23.xlsx"), sep.names = "_")
#
df_seasonal <- read.xlsx(paste0(SAF_directory, "/seasonal_output_aug_23.xlsx"),sep.names = "_")

# Scheme data is not included in SAF data at this point but useful to have later.

df_scheme <- read.xlsx(paste0(SAF_directory, "/scheme_claim_output_aug_23.xlsx"),sep.names = "_")



# # # Tidy SAF data ----------------------------------------------------------------
# #
# # #
# # Work with permanent and seasonal in a list
#
list_perm_seas <- list(df_permanent, df_seasonal)


# Clean names

list_perm_seas <- lapply(list_perm_seas, clean_names)

# Create line variable for list to index by LPID for permanent and seasonal. Number gives instance of each LPID.

list_perm_seas <- lapply(list_perm_seas, LPID_index)


#
# # Rename and create new variables for permanent and seasonal. Land_Use is also renamed here for ease, as are landusearea and bpsclaimedarea.
#
#
list_perm_seas <- lapply(list_perm_seas, rename_perm_seas_vars)


list_perm_seas <- lapply(list_perm_seas, new_perm_seas_vars)

# Create parish and holding from slc (permanent data) and mlc (seasonal data)

list_perm_seas[[1]]<-parishholdingperm(list_perm_seas[[1]])
list_perm_seas[[2]]<-parishholdingseas(list_perm_seas[[2]])



# # Clean names and create line variable for scheme data
#
df_scheme<-clean_names(df_scheme)

df_scheme <- LPID_index(df_scheme)


# rename and create new variables in scheme.

df_scheme <- rename_scheme_vars(df_scheme)

df_scheme <- new_scheme_vars(df_scheme)
#
# #
# #
# #
# # # Create dfs for missing obs ----------------------------------------------
# #
# # # Create dfs for missing records in permanent and seasonal.
#
#
missingperm_seas <- lapply(list_perm_seas, missing_obs)


# Missing field area in permanent and seasonal.

missfieldareaperm_seas <- lapply(list_perm_seas, missing_field_area)


# Missing eligible area in permanent and seasonal.

misseligibleareaperm_seas <- lapply(list_perm_seas, missing_eligible_area)


#
# # final list of permanent and seasonal removing blank records.
#
list_perm_seas <- lapply(list_perm_seas, cleaned_datasets)

# final scheme df removing blank records.


df_scheme <- cleaned_datasets(df_scheme)
#
# #
# #
# # Save to datashare
# #
# #
save(list_perm_seas, file = paste0(Code_directory, "/saflist_permseas_A_2023.rda"))

save(df_scheme, file = paste0(Code_directory, "/saf_scheme_A_2023.rda"))


# Save to ADM


write_dataframe_to_db(server=server,
                      database=database,
                      schema=schema,
                      table_name="saf_perm_A_2023",
                      dataframe=list_perm_seas[[1]],
                      append_to_existing = FALSE,
                      versioned_table=FALSE,
                      batch_size = 10000)



write_dataframe_to_db(server=server,
                      database=database,
                      schema=schema,
                      table_name="saf_seas_A_2023",
                      dataframe=list_perm_seas[[2]],
                      append_to_existing = FALSE,
                      versioned_table=FALSE,
                      batch_size = 10000)

write_dataframe_to_db(server=server,
                      database=database,
                      schema=schema,
                      table_name="saf_scheme_A_2023",
                      dataframe=df_scheme,
                      append_to_existing = FALSE,
                      versioned_table=FALSE,
                      batch_size = 10000)
#
#
#
#

# Import crofts data ------------------------------------------------------



# Import crofts data. Note: using read.csv here instead creates a df with HoldingID as the index, which we don't want!


# df_crofts <- read_csv(paste0(Croft_directory, "/ROC_Holdings_26-9-2023.csv"))
# 
# 
# # Clean names, remove Parish variable and create parish and holding.
# 
# df_crofts<-clean_names(df_crofts)
# 
# df_crofts <- subset(df_crofts, select = -(parish))
# 
# df_crofts <- df_crofts %>%
#   mutate(parish=
#            str_remove(substr(main_location_code, 1, 3), "123"),
#           holding=str_remove(substr(main_location_code, 5, 8), "123")
#   )
# 
# #
# #
# df_crofts<-df_crofts %>%
#   mutate(parish=as.numeric(parish),
#          holding=as.numeric(holding))
# 
# # Remove crofts with zero area
# 
# df_crofts <- df_crofts[df_crofts$total_area > 0, ]
# 
# # Remove crofts with invalid holding number.
# 
# 
# df_crofts <- df_crofts[df_crofts$holding != 0, ]
# 
# # Create variables for rented area and owned area
# 
# df_crofts <- df_crofts %>%
#   mutate(rented_area = ifelse(status_a == "Tenanted", total_area, 0),
#           owned_area = ifelse(status_a == "Owned", total_area, 0)
#          )
# 
# #
# # # Create new dataframe
# #
# #
# df_crofts <- subset(df_crofts, select = c(parish, holding, total_area, rented_area, owned_area))
# 
# 
# # Group by parish and holding. Note for future: make this into a function,
# 
# df_crofts<- df_crofts %>%
#   group_by(parish, holding) %>%
#   dplyr::summarise(
#     cc_tot_area = sum(total_area),
#     cc_rented_area = sum(rented_area),
#     cc_owned_area = sum(owned_area),
#     num_crofts = sum(total_area != 0),
#     num_rented_crofts = sum(rented_area != 0),
#     num_owned_crofts = sum(owned_area != 0),
#     .groups = "rowwise"
#   )
# # Remove row with NAs.
# 
# df_crofts<-df_crofts[complete.cases(df_crofts), ]
# # #
# 
# 
# save(df_crofts, file = paste0(Code_directory, "/crofts_A_2023.rda"))
# #
# #
# #
# #
# write_dataframe_to_db(server=server,
#                       database=database,
#                       schema=schema,
#                       table_name="crofts_A_2023",
#                       dataframe=df_crofts,
#                       append_to_existing = FALSE,
#                       versioned_table=FALSE,
#                       batch_size = 10000)
# 
# 
# 
# 
# 
# # Import ScotEID data -----------------------------------------------------------------
# 
# 
# # Import ScotEID data
# 
# df_scoteid<-read.csv(paste0(ScotEID_directory, "/Agricultural Statistics - June Census 2023 - Data - ScotEID - Cattle - 1st June.csv"))
# #
# 
# save(df_scoteid, file = paste0(Code_directory, "/scoteid_A_2023.rda"))
# #
# #
# #
# write_dataframe_to_db(server=server,
#                        database=database,
#                        schema=schema,
#                        table_name="scoteid_A_2023",
#                        dataframe=df_scoteid,
#                        append_to_existing = FALSE,
#                        versioned_table=FALSE,
#                        batch_size = 10000)

# # Import address file ------------------------------------------------------------
#
#
#
#
# # Import 1st June address file from RP&S
#
# df_addressfile<-read_sas(paste0(SAS_directory,"/address_occid_01jun23.sas7bdat"))
#
# save(df_addressfile, file = paste0(Code_directory, "/address_occid_01jun_2023.rda"))
#
# write_dataframe_to_db(server=server,
#                       database=database,
#                       schema=schema,
#                       table_name="address_occid_01jun_2023",
#                       dataframe=df_addressfile,
#                       append_to_existing = FALSE,
#                       versioned_table=FALSE,
#                       batch_size = 10000)

