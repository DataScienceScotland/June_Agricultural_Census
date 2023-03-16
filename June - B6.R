# This code applies corrections to the SAF datasets (permanent and seasonal). These corrections will include manual corrections which can be added each year, perhaps to the function script.
# Then, for the permanent data, it corrects invalid codes (this will need updating every year) and  splits up lines with both SFP code and Other Code. For the seasonal data, it splits up and removes those with only an Other code.
# Code corrections may need updating yearly depending on SAF data.
# This script is based on the code in B4 and B6 of the June Project (\\s0177a\datashare\seerad\ags\census\branch1\NewStructure\Surveys\June\Main\JUNE CENSUS PROJECT - 2021 Provisional Scott)

# Created by Lucy Nevard 27.01.23
# Modified by Lucy Nevard 28.02.23



# Before import -----------------------------------------------------------



# Clear environment prior

rm(list = ls())

# Load packages

library(tidyverse)
library(dplyr)


# Load functions

source("Functions/Functions.R")


# Datashare file path for import and export

Code_directory <- ("//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Codeconversion_2023")

# ADM schema for export

server <- "s0196a\\ADM"
database <- "RuralAndEnvironmentalScienceFarmingStatistics"
schema <- "juneagriculturalsurvey2023alpha"


# Import SAF data -------------------------------------------------------------


# In the code from 2021, the data used last in B6 was AUG - I use SEP here for consistency with rest of previous code.

list_perm_seas <- loadRData(paste0(Code_directory, "/saflist_permseas_A.rda"))


#  Apply corrections to both datasets.  -----------------------------------
# Correcting area variables and slc when blank. Code corrections for sfp_code and other_code.

# For SAF codes - this might change year to year. But these below might still be used.
# Manual corrections will also be here and change from year to year.
# Make these manual changes part of a function in the future. Then codes can be updated in function script.


list_perm_seas <- lapply(list_perm_seas, change_codes)


# Unlist permanent and seasonal.  ---------------------------------------


# Tidy this up in future.

#  Create final dfs for permanent, scheme and seasonal.

names(list_perm_seas) <- c("perm", "seas")

for (i in seq(list_perm_seas)) {
  assign(paste("saf", names(list_perm_seas)[[i]], sep = "_"), list_perm_seas[[i]])
}


saf_perm$landtype <- "PERM"
saf_seas$landtype <- "SEAS"


# Remove rows of all NAS. Couldn't figure out how to do this in a list.


saf_perm <- saf_perm[rowSums(is.na(saf_perm)) != ncol(saf_perm), ]

saf_seas <- saf_seas[rowSums(is.na(saf_seas)) != ncol(saf_seas), ]


# Permanent - split up SAF datalines into claimtype other or sfp --------

# Reformat df with claimtype as OTHER or SFP and "line" variable increasing by 0.01 if OTHER.
# LLO flags also assigned

# Other claimtype data

saf_permother <- filter(saf_perm, saf_perm$other_area > 0)

# 

saf_permother <- newvarsother(saf_permother)


# SFP claimtype data

saf_permsfp <- filter(saf_perm, saf_perm$sfp_area > 0)


saf_permsfp <- newvarssfp(saf_permsfp)


# Merge Other and SFP


saf_perm_final <- rbind(saf_permother, saf_permsfp)


# Remove unnecesary variables


saf_perm_final <- saf_perm_final %>%
  select(-c(other_area, other_code, prefix))

# Create correct parish and holding

saf_perm_final2 <- parishholdingslc(saf_perm_final)



# The SAS code here has tables to see what errors are left in the permanent df - not clear what we're looking for at this point and there aren't any fixes in the code, so I've left this out for now.


#  Seasonal - filter out "Other" holdings, keep only SFP  --------------------------------------------------------------
# According to desk notes, the data from Other is too messy to be reliable. The seasonal data is therefore an underestimate.


saf_seas <- filter(saf_seas, saf_seas$sfp_area > 0)

# SFP claimtype data

saf_seas<-newvarsseas(saf_seas)


# Create correct parish and holding

saf_seas<-parishholdingmlc(saf_seas)

# Save separate permanent and seasonal datasets --------------------------

# Save to datashare

save(saf_perm, file = paste0(Code_directory, "/saf_perm_B6.rda"))

save(saf_seas, file = paste0(Code_directory, "/saf_seas_B6.rda"))



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
