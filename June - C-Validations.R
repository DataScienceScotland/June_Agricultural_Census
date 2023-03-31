library(tidyverse)
#yr = this year 20xx
yr <- 21
yr1 <- yr-1 
yr2 <-  yr-2
yr3 <-  yr-3
yr_list <- c(yr, yr1, yr2)




# Before import -----------------------------------------------------------


# Clear environment prior
rm= ls()

# Datashare file path for import and export

Code_directory <-
  ("//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Codeconversion_2023")

# ADM schema for export

server <- "s0196a\\ADM"
database <- "RuralAndEnvironmentalScienceFarmingStatistics"
schema <- "juneagriculturalsurvey2023alpha"