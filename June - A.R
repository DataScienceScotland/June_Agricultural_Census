# This script imports the datasets from SAF, Ags (the census returns) from this year and the previous year, and the Crofting Commission. They are saved as rdas in the datashare 
# Please update the directories at the top to get the correct years.
# Created by Lucy Nevard 24.02.22 
# Modified by Lucy Nevard 27.02.22



# Before import -----------------------------------------------------------


# Clear environment prior 

rm(list=ls())

# Data drop file paths for import

SAF_directory <- ("//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Main/June21/Data Drops/SAF")

AGS_directory <- ("//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Main/June21/Data Drops/AGS")

AGS_prev_directory <- ("//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Main/June20/Data Drops/AGS/2019")

Croft_directory<-("//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Main/June21")

# Datashare file path for export

Code_directory <- ("//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Codeconversion_2023")

# ADM schema for export

server <- "s0196a\\ADM"
database <- "RuralAndEnvironmentalScienceFarmingStatistics"
schema <- "juneagriculturalsurvey2023alpha"

# Load packages

library(readxl)
library(data.table)
library(dplyr)
library(stringr)
library(tidyverse)
library(RtoSQLServer)

# Load functions

source("Functions/A1 Functions.R")


#schema <- "agriculture"


# Import datasets separately (or do this directly into a list depending on what other files are in the folder).

df_permanent<-read_excel(paste0(SAF_directory, "/permanent_output1_SecondDrop.xlsx"))  # called script1 in SAS
df_scheme<-read_excel(paste0(SAF_directory,"/scheme_output1_SecondDrop.xlsx"))  # called script2 in SAS
df_seasonal<-read_excel(paste0(SAF_directory,"/seasonal_output1_SecondDrop.xlsx")) # called script3 in SAS



# SAF data ----------------------------------------------------------------


list_perm_seas<-list(df_permanent, df_seasonal)

# Create line variable for list to index by LPID for permanent and seasonal. Number gives instance of each LPID. 

list_perm_seas<-lapply(list_perm_seas, LPID_index)

# Create new df and line variable for scheme

df_scheme<- LPID_index(df_scheme)


# Rename and create variables ---------------------------------------------

# Rename and create new variables for permanent and seasonal. Land Use is also renamed here for ease, as are landusearea and bpsclaimedarea.


list_perm_seas<-lapply(list_perm_seas, rename_perm_seas_vars)

list_perm_seas<-lapply(list_perm_seas, new_perm_seas_vars)

# rename and create new variables in scheme.

df_scheme<- rename_scheme_vars(df_scheme)

df_scheme<-new_scheme_vars(df_scheme)



# Create dfs for missing obs ----------------------------------------------

# Create dfs for missing observations in permanent and seasonal. 
# Note: this is looking for NAs rather than full stops or blanks as in the SAS code. R gives blanks as NAs, unlike in SAS. 

missingperm_seas<-lapply(list_perm_seas, missing_obs)


# Missing field area in permanent and seasonal. Again, we are looking for NAs and 0s (rather than full stops and 0s as in SAS).

missfieldareaperm_seas<-lapply(list_perm_seas, missing_field_area)


# Missing eligible area in permanent and seasonal.

misseligibleareaperm_seas<-lapply(list_perm_seas, missing_eligible_area)

  misseligibleareaperm_seas<-lapply(list_perm_seas, function(x)
    
  {x[is.na(eligible_area)]
  })


# Write these to rda (for future use)
# 
# setwd("C:/Users/u455049/Documents/R/repos/June")
# 
# save(missingperm_seas, file=paste("missingallsaf",yr, "perm_seas", mnth, sep='_',".rda"))
# save(missfieldareaperm_seas, file=paste("missingfieldareaallsaf",yr, "perm_seas", mnth, sep='_',".rda"))
# save(misseligibleareaperm_seas, file=paste("missingeligibleareaallsaf",yr, "perm_seas", mnth, sep='_',".rda"))


# Create final dataframes -------------------------------------------------

# final list of permanent and seasonal removing blank records. 

list_perm_seas<-lapply(list_perm_seas, cleaned_datasets)

# final scheme df removing blank records. 


df_scheme<-cleaned_datasets(df_scheme)


#  Create final dfs for permanent, scheme and seasonal. 

names(list_perm_seas) <- c("perm", "seas")

for (i in seq(list_perm_seas))
  assign(paste("allsaf", names(list_perm_seas)[[i]], sep = "_"), list_perm_seas[[i]])




save(allsaf_perm, file=paste0(Code_directory, "/allsaf_perm_A.rda"))
save(allsaf_seas, file=paste0(Code_directory, "/allsaf_seas_A.rda"))
save(df_scheme, file=paste0(Code_directory, "/allsaf_scheme_A.rda"))


# allsaf_perm<- allsaf_perm%>%
#   mutate_all(str_sub, 1, 49)


 # allsaf_perm<- as.data.frame(unclass(allsaf_perm),stringsAsFactors=TRUE)

# The following code to write to ADM returns error - find out how to fix this and then repeat this for all datasets. 

write_dataframe_to_db(server=server, 
                      database=database, 
                      schema=schema, 
                      table_name="allsaf_perm_A", 
                      dataframe=allsaf_perm, 
                      append_to_existing = FALSE,
                      batch_size=1000, 
                      versioned_table=FALSE)


# Remove SAF data to free up memory 

rm(df_permanent, df_seasonal, allsaf_perm, allsaf_seas, df_scheme)


# Ags data ----------------------------------------------------------------

# Remove two variables from df_SAF. This should be changed to keep all necessary variables

df_SAF<-subset(df_SAF, select = -c(item21310, X))

# Change variable types


df_SAF<-df_SAF %>%
  mutate(
    item186=as.character(item186)
  )

# Check variable types are correct

str(df_SAF, list.len=ncol(df_SAF))



# Formatting non-SAF df ---------------------------------------------------

df_nonSAF<-subset(df_nonSAF, select = -c(item21310, X))

df_nonSAF<-df_nonSAF %>%
  mutate(
    item185=as.character(item185),
    item186=as.character(item186)
  )

# Check variables types are correct

str(df_nonSAF, list.len=ncol(df_nonSAF))

# Save rdas

save(df_SAF, file=paste0(Code_directory, "/Ags_SAF_A.rda"))
save(df_nonSAF, file=paste0(Code_directory, "/Ags_nonSAF_A.rda"))


# Ags data previous year --------------------------------------------------



df_SAFprev<-read.csv(paste0(AGS_prev_directory,"/SAF1011.csv"))
df_nonSAFprev<-read.csv(paste0(AGS_prev_directory,"/NonSAF1011.csv"))



df_SAFprev<-subset(df_SAFprev, select = -c(item9901, X, X.1))

# Change variable types


df_SAFprev<-df_SAFprev %>%
  mutate(
    survtype=as.character(survtype),
    submisType=as.character(submisType),
    maderight=as.character(maderight),
    item186=as.character(item186)
  )

# Check variable types are correct

str(df_SAFprev, list.len=ncol(df_SAFprev))



# Formatting non-SAF df ---------------------------------------------------

df_nonSAFprev<-subset(df_nonSAFprev, select = -c(item9901, X))

df_nonSAF<-df_nonSAF %>%
  mutate(
    survtype=as.character(survtype),
    submisType=as.character(submisType),
    maderight=as.character(maderight),
    item185=as.character(item185),
    item186=as.character(item186)
  )

# Check variables types are correct

str(df_nonSAFprev, list.len=ncol(df_nonSAFprev))



# Save rdas

save(df_SAFprev, file=paste0(Code_directory, "/Ags_SAFprev_A.rda"))
save(df_nonSAFprev, file=paste0(Code_directory, "/Ags_nonSAFprev_A.rda"))


# Save to ADM



# Crofting data -----------------------------------------------------------

# Import crofts data. Note: using read.csv here creates a df with HoldingID as the index, which we don't want!


df_crofts<-read_csv(paste0(Croft_directory, "/Register_of_crofts_Holdings_16-9-2021.csv"))



# Remove Parish variable and create parish and holding. Note: need to deal with mainlocationcode when they have county in 

df_crofts<-subset(df_crofts, select = -(Parish))

df_crofts$parish<-str_remove(substr(df_crofts$MainLocationCode,1,3), "123")
df_crofts$holding <- str_remove(substr(df_crofts$MainLocationCode,5,8),"123")


df_crofts$parish<-as.numeric(df_crofts$parish)
df_crofts$holding<-as.numeric(df_crofts$holding)

# Remove crofts with zero area

df_crofts<-df_crofts[df_crofts$TotalArea>0,]

# Remove crofts with invalid holding number. This removes more records than in SAS - blanks are removed by R but not in the SAS code. I assume holdings with blanks should be removed. 


df_crofts<-df_crofts[df_crofts$holding!=0,]

# Create variables for rented area and owned area

df_crofts<-mutate(df_crofts, rentedarea=ifelse(StatusA=="Tenanted",TotalArea,0))
df_crofts<-mutate(df_crofts, ownedarea=ifelse(StatusA=="Owned",TotalArea,0))


# Create new dataframe


df_crofts<- subset(df_crofts, select = c(parish, holding, TotalArea, rentedarea, ownedarea))





df_croftsfinal <- df_crofts %>%
  group_by(parish, holding) %>%
  summarise (CCTotArea = sum(TotalArea),
             CCRentedArea=sum(rentedarea),
             CCOwnedArea=sum(ownedarea),
             numcrofts=sum(TotalArea != 0),
             numRentedCrofts=sum(rentedarea != 0),
             numOwnedCrofts=sum(ownedarea != 0),
             .groups = "rowwise")
# Check mising values are set to zero.

# Save to datashare

save(df_croftsfinal, file=paste0(Code_directory, "/crofts_A.rda"))
                    