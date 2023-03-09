# This script imports and formats the crofts dataset - code is rewrite of "A3 - Load CC data" in the June SAS code (\\s0177a\datashare\seerad\ags\census\branch1\NewStructure\Surveys\June\Main\JUNE CENSUS PROJECT - 2021 Provisional Scott)
# This may need adapting each year as CC are not usually consistent in their data formatting
# Created by Lucy Nevard 13.01.23 
# Modified by Lucy Nevard 24.01.23


# Before import -----------------------------------------------------------


# Clear environment prior 

rm(list=ls())

# Load packages

library("readr")
library("stringr")
library("dplyr")
library("tidyverse")

# Assign year

yr<-21

# Import crofts data. Note: using read.csv here creates a df with HoldingID as the index, which we don't want!

df_crofts<-read_csv("//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Main/June21/Register_of_crofts_Holdings_16-9-2021.csv")

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


df_croftsfinal <- subset(df_crofts, select = c(parish, holding, TotalArea, rentedarea, ownedarea))





df_croftsfinal <- df_croftsfinal %>%
  group_by(parish, holding) %>%
  summarise (CCTotArea = sum(TotalArea),
             CCRentedArea=sum(rentedarea),
             CCOwnedArea=sum(ownedarea),
             numcrofts=nrow(TotalArea),
             .groups = "rowwise")
  

#save(df_SAF, file=paste("SAF_ags",yr, sep='_',".rda"))