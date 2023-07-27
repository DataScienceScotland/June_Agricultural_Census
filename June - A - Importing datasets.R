# This script imports the datasets from SAF, Ags (the census returns) from this year and the previous year, and the Crofting Commission. They are saved as rdas in the datashare and tables on the ADM server.
# Update the directories at the top to get data drops from the correct years.
# The data used currently is from June 2023, only SAF and Ags data available at the moment.
# # Running this to import the SAF data cut 2 (28.06.23) and a new Ags data cut 29.06.23
# Edit to bring in CTS data here as well.
# This script is Based on programs A1, A2, A2.(a) and A3 in the June SAS project (\\s0177a\datashare\seerad\ags\census\branch1\NewStructure\Surveys\June\Main\JUNE CENSUS PROJECT - 2021 Provisional)
# Created by Lucy Nevard 24.02.23 (original individual scripts created 22.11.22),
# Modified by Lucy Nevard 09.06.23



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

# Load functions

source("Functions/Functions.R")

# Data drop file paths for import

SAF_directory <- ("//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Main/June23/Data Drops/SAF")
AGS_directory <- ("//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Main/June23/Data Drops/AGS")
#AGS_prev_directory <- ("//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Main/June20/Data Drops/AGS/2019") # This is the exact directory used in the SAS project for the 2021 analysis. Unclear why it's in a 2019 folder.
#Decide when to bring in 2021 Ags data - probably not necessary at this stage. All code regarding prev year's data has been commented out. 
Croft_directory <- ("//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Main/June23/Data Drops/Crofting Commission")

# Datashare file path for export

Code_directory <- ("//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Codeconversion_2023/2023")

# ADM schema for export

server <- "s0196a\\ADM"
database <- "RuralAndEnvironmentalScienceFarmingStatistics"
schema <- "juneagriculturalsurvey2023alpha"

# schema <- "agriculture"  # for the general schema


# Import data -------------------------------------------------------------
# 
# 
# # Import SAF datasets separately (or do this directly into a list depending on what other files are in the folder). Filename will be different for the data drops in June and September.
# 
#  df_permanent <- read.xlsx(paste0(SAF_directory, "/permanent_output2.xlsx"), sep.names = "_") # called script1 in SAS
# #
#  df_seasonal <- read.xlsx(paste0(SAF_directory, "/season_output2.xlsx"),sep.names = "_") # called script3 in SAS # In previous years, this was called seasonal. In 2023, it is called season
# 
# # Scheme data is not included in SAF data at this point but useful to have.
# 
# df_scheme <- read.xlsx(paste0(SAF_directory, "/scheme_output2.xlsx"),sep.names = "_") # called script2 in SAS
# 
# # Import Ags data (downloaded from Ags). From 2023 onwards, there will be just one datafile from Ags (no SAF or NonSAF).


df_nonSAF<-read.csv(paste0(AGS_directory,"/June_extract_270723.csv"),
                     fileEncoding="latin1")  # There is no SAF/non-SAF from 2023 onwards. All forms are essentially equivalent to non-SAF.

# The previous year's data will still be SAF and NonSAF in 2023 (i.e. 2021 data). 2024 onwards, the previous year will be 2023 so only one datafile.

# df_SAFprev <- read.csv(paste0(AGS_prev_directory, "/SAF1011.csv"))
# df_nonSAFprev <- read.csv(paste0(AGS_prev_directory, "/NonSAF1011.csv"))

# # Import crofts data. Note: using read.csv here creates a df with HoldingID as the index, which we don't want! 
# # Crofts data isn't added to the census dataset until the very end of the process (see E2 in SAS project).
# 
 # df_crofts <- read_csv(paste0(Croft_directory, "/ROC_Holdings_10-7-2023.csv"))
# 
# # Permanent and seasonal ----------------------------------------------------------------
# 
# # 
# # Work with permanent and seasonal in a list
# 
# list_perm_seas <- list(df_permanent, df_seasonal)
# 
# 
# # Clean names to conform with good R practice
# 
# list_perm_seas <- lapply(list_perm_seas, clean_names)
# 
# # Create line variable for list to index by LPID for permanent and seasonal. Number gives instance of each LPID.
# 
# list_perm_seas <- lapply(list_perm_seas, LPID_index)
# 
# 
# 
# # Rename and create new variables for permanent and seasonal. Land_Use is also renamed here for ease, as are landusearea and bpsclaimedarea.
# 
# 
# list_perm_seas <- lapply(list_perm_seas, rename_perm_seas_vars)
# 
# 
# list_perm_seas <- lapply(list_perm_seas, new_perm_seas_vars)
# 
# # Create parish and holding from slc (permanent data) and mlc (seasonal data)
# 
# list_perm_seas[[1]]<-parishholdingperm(list_perm_seas[[1]])
# list_perm_seas[[2]]<-parishholdingseas(list_perm_seas[[2]])
# 
# 
# # Scheme data -------------------------------------------------------------
# 
# 
# # Clean names and create line variable for scheme data
# 
# df_scheme<-clean_names(df_scheme)
# 
# df_scheme <- LPID_index(df_scheme)
# 
# 
# # rename and create new variables in scheme.
# 
# df_scheme <- rename_scheme_vars(df_scheme)
# 
# df_scheme <- new_scheme_vars(df_scheme)
# 
# 
# 
# 
# # Create dfs for missing obs ----------------------------------------------
# 
# # Create dfs for missing observations in permanent and seasonal.
# # Note: this is looking for NAs rather than full stops or blanks as in the SAS code. R gives blanks as NAs, unlike in SAS.
# 
# missingperm_seas <- lapply(list_perm_seas, missing_obs)
# 
# 
# # Missing field area in permanent and seasonal. Again, we are looking for NAs and zeroes (rather than full stops and 0s as in SAS).
# 
# missfieldareaperm_seas <- lapply(list_perm_seas, missing_field_area)
# 
# 
# # Missing eligible area in permanent and seasonal. This looks for just NAs, not zeroes (as in SAS project, is there a good reason for this?)
# 
# misseligibleareaperm_seas <- lapply(list_perm_seas, missing_eligible_area)
# 
# 
# 
# 
# 
# # Create final dataframes -------------------------------------------------
# 
# # final list of permanent and seasonal removing blank records.
# 
# list_perm_seas <- lapply(list_perm_seas, cleaned_datasets)
# 
# # final scheme df removing blank records.
# 
# 
# df_scheme <- cleaned_datasets(df_scheme)

#
#
#
# # # Tidy Ags data ----------------------------------------------------------------
# #
# 
# #
#
# # # Formatting non-SAF df ---------------------------------------------------
#
df_nonSAF <- subset(df_nonSAF, select = -c(item21310, X))
#
#
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

#
# # Ags data previous year --------------------------------------------------
# 
# # Does this section need to be here, or can it be moved to Section D - pre-imputation ?
# 
# df_SAFprev <- subset(df_SAFprev, select = -c(item9901, X, X.1))
# 
# # Check variable types are correct
# 
# str(df_SAFprev, list.len = ncol(df_SAFprev))
# 
# # Change variable types as needed
# 
# df_SAFprev <- df_SAFprev %>%
#   mutate(
#     survtype = as.character(survtype),
#     submisType = as.character(submisType),
#     maderight = as.character(maderight),
#     item186 = as.character(item186)
#   )
# 
# 
# 
# 
# 
# # Formatting non-SAF df prev ---------------------------------------------------
# 
# df_nonSAFprev <- subset(df_nonSAFprev, select = -c(item9901, X))
# 
# # Check variables types are correct
# 
# str(df_nonSAFprev, list.len = ncol(df_nonSAFprev))
# 
# # Change variable types as needed
# 
# df_nonSAF <- df_nonSAF %>%
#   mutate(
#     survtype = as.character(survtype),
#     submisType = as.character(submisType),
#     maderight = as.character(maderight),
#     item185 = as.character(item185),
#     item186 = as.character(item186)
#   )
# 
# 
# 
# 
# 
# # Crofting data -----------------------------------------------------------
# 
#
# # Clean names, remove Parish variable and create parish and holding. Note: need to deal with mainlocationcode when they have county in
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
# 
# 
# df_crofts<-df_crofts %>%
#   mutate(parish=as.numeric(parish),
#          holding=as.numeric(holding))
# 
# # Remove crofts with zero area
# 
# df_crofts <- df_crofts[df_crofts$total_area > 0, ]
# 
# # Remove crofts with invalid holding number. This removes more records than in SAS - blanks are removed by R but not in the SAS code. I assume holdings with blanks should be removed.
# 
# 
# df_crofts <- df_crofts[df_crofts$holding != 0, ]
# 
# # Create variables for rented area and owned area
# 
# df_crofts <- df_crofts %>%
#   mutate(rented_area = ifelse(status_a == "Tenanted", total_area, 0),
#           owned_area = ifelse(status_b == "Owned", total_area, 0)
#          )
# 
# 
# # Create new dataframe
# 
# 
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
# 



# Export datasets -----------------------------------------------------------


# Save to datashare
# 
# 
# save(list_perm_seas, file = paste0(Code_directory, "/saflist_permseas_A_2023.rda"))
# 
# save(df_scheme, file = paste0(Code_directory, "/saf_scheme_A_2023.rda"))


# # Save rdas
# 

save(df_nonSAF, file = paste0(Code_directory, "/Ags_A_2023.rda"))

# # Save rdas
# 
# save(df_SAFprev, file = paste0(Code_directory, "/Ags_SAFprev_A.rda"))
# save(df_nonSAFprev, file = paste0(Code_directory, "/Ags_nonSAFprev_A.rda"))
# 
# 
# 
 # save(df_crofts, file = paste0(Code_directory, "/crofts_A_2023.rda"))




# Save to ADM


# write_dataframe_to_db(server=server,
#                       database=database,
#                       schema=schema,
#                       table_name="saf_perm_A_2023",
#                       dataframe=list_perm_seas[[1]],
#                       append_to_existing = FALSE,
#                       versioned_table=FALSE,
#                       batch_size = 10000)
# 
# 
# 
# write_dataframe_to_db(server=server,
#                       database=database,
#                       schema=schema,
#                       table_name="saf_seas_A_2023",
#                       dataframe=list_perm_seas[[2]],
#                       append_to_existing = FALSE,
#                       versioned_table=FALSE,
#                       batch_size = 10000)
# 
# write_dataframe_to_db(server=server,
#                       database=database,
#                       schema=schema,
#                       table_name="saf_scheme_A_2023",
#                       dataframe=df_scheme,
#                       append_to_existing = FALSE,
#                       versioned_table=FALSE,
#                       batch_size = 10000)



write_dataframe_to_db(server=server,
                      database=database,
                      schema=schema,
                      table_name="Ags_A_2023",
                      dataframe=df_nonSAF,
                      append_to_existing = FALSE,
                      versioned_table=FALSE,
                      batch_size = 10000)

# 
# 
# write_dataframe_to_db(server=server,
#                       database=database,
#                       schema=schema,
#                       table_name="Ags_SAF_prev_A",
#                       dataframe=df_SAFprev,
#                       append_to_existing = FALSE,
#                       versioned_table=FALSE,
#                       batch_size = 10000)
# 
# 
# write_dataframe_to_db(server=server,
#                       database=database,
#                       schema=schema,
#                       table_name="Ags_nonSAF_prev_A",
#                       dataframe=df_nonSAFprev,
#                       append_to_existing = FALSE,
#                       versioned_table=FALSE,
#                       batch_size = 10000)
# 
# 
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
