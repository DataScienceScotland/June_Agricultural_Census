# This script imports and formats AGS (JAC) data extracts from the previous year - code is rewrite of "A2. (a) - Ags data extract upload -prev. year" in the June SAS code (\\s0177a\datashare\seerad\ags\census\branch1\NewStructure\Surveys\June\Main\JUNE CENSUS PROJECT - 2021 Provisional Scott)
# Created by Lucy Nevard 13.01.23
# Modified by Lucy Nevard 13.01.23


# Before import -----------------------------------------------------------


# Clear environment prior

rm(list = ls())

# Load packages

library(tidyverse)

# Set working directory


setwd("//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Main/June20/Data Drops/AGS/2019")


#  Assign year to allow creation of dataset with correct year (at the very end of this script). This will be the previous year to the current dataset.

yr <- 20


# Import ------------------------------------------------------------------

# This imports the final version of these datasets - change to version/date needed.


df_SAF <- read.csv("SAF1011.csv")
df_nonSAF <- read.csv("NonSAF1011.csv")


# Formatting SAF df -------------------------------------------------------


# Remove  variables from df_SAF. This should be changed to keep all necessary variables

df_SAF <- subset(df_SAF, select = -c(item9901, X, X.1))

# Change variable types


df_SAF <- df_SAF %>%
  mutate(
    survtype = as.character(survtype),
    submisType = as.character(submisType),
    maderight = as.character(maderight),
    item186 = as.character(item186)
  )

# Check variable types are correct

str(df_SAF, list.len = ncol(df_SAF))



# Formatting non-SAF df ---------------------------------------------------

df_nonSAF <- subset(df_nonSAF, select = -c(item9901, X))

df_nonSAF <- df_nonSAF %>%
  mutate(
    survtype = as.character(survtype),
    submisType = as.character(submisType),
    maderight = as.character(maderight),
    item185 = as.character(item185),
    item186 = as.character(item186)
  )

# Check variables types are correct

str(df_nonSAF, list.len = ncol(df_nonSAF))


# Save RDAs for each df into repo or other directory

setwd("C:/Users/u455049/Documents/R/repos/June")

save(df_SAF, file = paste("SAF_ags", yr, sep = "_", ".rda"))

save(df_nonSAF, file = paste("nonSAF_ags", yr, sep = "_", ".rda"))
