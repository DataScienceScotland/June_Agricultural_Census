# This code applies corrections to the SAF datasets (permanent and seasonal). These corrections will include manual corrections which can be added each year, perhaps to the function script.
# Then, for the permanent data, it corrects invalid codes (this will need updating every year) and  splits up lines with both SFP code and Other Code. For the seasonal data, it splits up and removes those with only an Other code.
# Code corrections may need updating yearly depending on SAF data.
# This script is based on the code in B4 and B6 of the June Project (\\s0177a\datashare\seerad\ags\census\branch1\NewStructure\Surveys\June\Main\JUNE CENSUS PROJECT - 2021 Provisional Scott)

# Created by Lucy Nevard 27.01.23 
# Modified by Lucy Nevard 27.02.23


# Before import -----------------------------------------------------------



# Clear environment prior 

rm(list=ls())


# Datashare file path for import and export

Code_directory <- ("//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Codeconversion_2023")

# ADM schema for export

server <- "s0196a\\ADM"
database <- "RuralAndEnvironmentalScienceFarmingStatistics"
schema <- "juneagriculturalsurvey2023alpha"

# Load packages

library(tidyverse)
library(dplyr)

# Load functions

source("Functions/Functions.R")




# Import SAF data -------------------------------------------------------------


# In the code from 2021, the data used last in B6 was AUG - I use SEP here for consistency with rest of previous code.

allsaf_perm<-loadRData(paste0(Code_directory, "/allsaf_perm_A.rda"))
allsaf_seas<-loadRData(paste0(Code_directory, "/allsaf_seas_A.rda"))


list_perm_seas<-list(allsaf_perm,allsaf_seas)



#  Apply corrections to both datasets.  -----------------------------------
# Correcting area variables and slc when blank. Code corrections for sfp_code and other_code. 

# For SAF codes - this might change year to year. But these below might still be used. 
# Manual corrections will also be here and change from year to year. 
# Make these manual changes part of a function in the future. Then codes can be updated in function script. 


  
list_perm_seas<-lapply(list_perm_seas, function(x)
  {mutate (x,
          other_area=
            ifelse(land_use_area<bps_claimed_area,"0",other_area),
          sfp_area=
            ifelse(land_use_area<bps_claimed_area,bps_claimed_area,sfp_area),
          land_use_area=
            ifelse(land_use_area<bps_claimed_area,bps_claimed_area,land_use_area),
          slc= ifelse(slc=="", mlc, slc),
          sfp_code = 
            recode(as.factor(sfp_code),"TREE" = 'TREES',"UNSP" = 'EXCL'),
          sfp_code = 
            recode(as.factor(other_code),"TREE" = 'TREES',"UNSP" = 'EXCL'))})







# Unlist permanent and seasonal.  ---------------------------------------


# Tidy this up in future.


for (i in seq(list_perm_seas))
  assign(paste0("df", i), list_perm_seas[[i]])

allsaf_perm<-df1
allsaf_seas<-df2

rm(df1,df2)


allsaf_perm$landtype<-"PERM"
allsaf_seas$landtype<-"SEAS"


# Remove rows of all NAS.


allsaf_perm<-allsaf_perm[rowSums(is.na(allsaf_perm)) != ncol(allsaf_perm), ]

allsaf_seas<-allsaf_seas[rowSums(is.na(allsaf_seas)) != ncol(allsaf_seas), ]



# Permanent - split up SAF datalines into claimtype other or sfp --------

# Reformat df with claimtype as OTHER or SFP and "line" variable increasing by 0.01 if OTHER. 
# LLO flags also assigned 

# Other claimtype data

allsaf_permother<-filter(allsaf_perm, allsaf_perm$other_area>0)

allsaf_permother$claimtype<-"OTHER"  

# Tidy these bits up and make functions

allsaf_permother$prefix<-substr(allsaf_permother$other_code,1,4)

allsaf_permother<-allsaf_permother %>%
  mutate(
    code = other_code,
    crops = other_code,
    area = other_area
  )


allsaf_permother$other_code<-as.character(allsaf_permother$other_code)


allsaf_permother <- allsaf_permother %>%
  mutate(
    LLO=
      ifelse(prefix=="LLO-","Y",ifelse(other_code=="LLO","Y","N")),
    other_code=
      ifelse(prefix=="LLO-", substring(other_code, 5),other_code),
    other_code=
      ifelse(other_code=="", "OTH-LAND",other_code))



# SFP claimtype data

allsaf_permsfp<-filter(allsaf_perm, allsaf_perm$sfp_area>0)

allsaf_permsfp$sfp_code<-as.character(allsaf_permsfp$sfp_code)

allsaf_permsfp<-allsaf_permsfp %>%
  mutate(
    claimtype = "SFPS",
    prefix = substr(sfp_code,1,4),
    code = sfp_code,
    crops = sfp_code,
    area = sfp_area
  )


allsaf_permsfp<- allsaf_permsfp %>%
  mutate(
    LLO=
      ifelse(prefix=="LLO-","1",ifelse(sfp_code=="LLO","Y","N")),
    sfp_code=
      ifelse(prefix=="LLO-", substring(sfp_code, 5),sfp_code),
    sfp_code=
      ifelse(sfp_code=="", "OTH-LAND",sfp_code),
    line= 
      ifelse(other_area>0, line+0.01, line))



# Merge Other and SFP


allsaf_perm <- rbind(allsaf_permother,allsaf_permsfp)


# Remove unneccesary variables


allsaf_perm<-allsaf_perm %>%
  select(-c(other_area,other_code,prefix))

# Create correct parish and holding 

allsaf_perm<-allsaf_perm %>%
  mutate(
    parish = str_remove(substr(slc,1,3), "^0+"),
    holding = str_remove(substr(slc,5,8),"^0+"),
  )


# The SAS code here has tables to see what errors are left in the permanent df - not clear what wer'e looking for at this point and there aren't any fixes in the code, so I've left this out for now. 




#  Seasonal - filter out "Other" holdings, keep only SFP  --------------------------------------------------------------
# According to desk notes, the data from Other is too messy to be reliable. The seasonal data is therefore an underestimate.



allsaf_seas<-filter(allsaf_seas, allsaf_seas$sfp_area>0)

# SFP claimtype data

allsaf_seas<-allsaf_seas %>%
  mutate(
    claimtype = "SFPS",
    LLO="N",
    code = sfp_code,
    crops = sfp_code,
    area = sfp_area,
    slc= ifelse(slc=="", mlc, slc)
  )


# Create correct parish and holding

allsaf_seas<-allsaf_seas%>%
  mutate(
    parish = str_remove(substr(mlc,1,3), "^0+"),
    holding = str_remove(substr(mlc,5,8),"^0+"),
  )


# Save separate permanent and seasonal datasets --------------------------

# Save to datashare

save(allsaf_perm,file=paste0(Code_directory, "/allsaf_perm_B6.rda"))

save(allsaf_seas,file=paste0(Code_directory, "/allsaf_seas_B6.rda"))



# Save to ADM
# 
# 
# write_dataframe_to_db(server=server, 
#                       database=database, 
#                       schema=schema, 
#                       table_name="allsaf_perm_B6", 
#                       dataframe=allsaf_perm, 
#                       append_to_existing = FALSE,
#                       batch_size=1000, 
#                       versioned_table=FALSE)
