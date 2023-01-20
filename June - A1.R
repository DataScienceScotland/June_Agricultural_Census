# This script collates the SAF data supplied by RPID - code is rewrite of A1 section of the June SAS code (\\s0177a\datashare\seerad\ags\census\branch1\NewStructure\Surveys\June\Main\JUNE CENSUS PROJECT - 2021 Provisional Scott)
# Created by Lucy Nevard 22.11.22 
# Modified by Lucy Nevard 22.12.22



# Before import -----------------------------------------------------------


# Clear environment prior 

rm(list=ls())

#SAF Data drop file path

SAF_directory <- ("//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Main/June21/Data Drops/SAF")

# Load packages

library(readxl)
library(data.table)
library(dplyr)
library(stringr)

#Load functions
source("Functions/A1 Functions.R")

#  Assign year and month to allow creation of dataset with correct year (at the very end of this script)
# Depending on how data stored, year/month may need to be specified in readxl

yr<-21
mnth<-"SEP"




# Import ------------------------------------------------------------------


# Import datasets separately (or do this directly into a list depending on what other files are in the folder).

df_permanent<-read_excel(paste0(SAF_directory, "/permanent_output1_SecondDrop.xlsx"))  # called script1 in SAS
df_scheme<-read_excel(paste0(SAF_directory,"/scheme_output1_SecondDrop.xlsx"))  # called script2 in SAS
df_seasonal<-read_excel(paste0(SAF_directory,"/seasonal_output1_SecondDrop.xlsx")) # called script3 in SAS




# Create list and index by LPID -------------------------------------------


# Create list for permanent and seasonal sheets - keep scheme separate as it has different variables. Is there a better way to work with them all in a list?

list_perm_seas<-list(df_permanent, df_seasonal)
names(list_perm_seas) <- c("perm", "seas")


# Create line variable for list to index by LPID (land parcel ID) for permanent and seasonal. Number gives instance of each LPID. 

list_perm_seas<-lapply(list_perm_seas, LPID_index)

# Create new df and line variable for scheme

df_schemework<- LPID_index(df_scheme)


# Rename and create variables ---------------------------------------------

# Rename and create new variables for permanent and seasonal. Land Use is also renamed here for ease, as are landusearea and bpsclaimedarea.


list_perm_seas<-lapply(list_perm_seas, rename_perm_seas_vars)

list_perm_seas<-lapply(list_perm_seas, new_perm_seas_vars)

# rename and create new variables in scheme.

df_schemework<- rename_scheme_vars(df_schemework)

df_schemework<-new_scheme_vars(df_schemework)



# Create dfs for missing obs ----------------------------------------------

# Create dfs for missing observations in permanent and seasonal. 
# Note: this is looking for NAs rather than full stops or blanks as in the SAS code. R gives blanks as NAs, unlike in SAS. 

missingperm_seas<-lapply(list_perm_seas, missing_obs)


# Missing field area in permanent and seasonal. Again, we are looking for NAs and 0s (rather than full stops and 0s as in SAS).

missfieldareaperm_seas<-lapply(list_perm_seas, missing_field_area)


# Missing eligible area in permanent and seasonal.

misseligibleareaperm_seas<-lapply(list_perm_seas, missing_eligible_area)

# Create final dataframes -------------------------------------------------

# final list of permanent and seasonal removing blank records. 

list_perm_seas<-lapply(list_perm_seas, cleaned_datasets)

# final scheme df removing blank records. 


df_schemework<-cleaned_datasets(df_schemework)


#  Create final dfs for permanent, scheme and seasonal. 


for (i in seq(list_perm_seas))
  assign(paste("allsaf", yr, names(list_perm_seas)[[i]], mnth, sep = "_"), list_perm_seas[[i]])

assign(paste("allsaf", yr, "scheme", mnth, sep = '_'), df_schemework) 


# This is where the A1 section of the SAS code ends.


# Write to csv ------------------------------------------------------------


# Write dfs to csv for use in further scripts
# Note: year and month are specified in the object name already...

output_path <- "C:/Users/z620777/OneDrive - SCOTS Connect/R/June"


write.csv(allsaf_21_perm_SEP, paste0(output_path, "/allsaf_21_perm_SEP.csv"))
write.csv(allsaf_21_scheme_SEP, paste0(output_path, "/allsaf_21_scheme_SEP.csv"))
write.csv(allsaf_21_seas_SEP, paste0(output_path, "/allsaf_21_seas_SEP.csv"))






