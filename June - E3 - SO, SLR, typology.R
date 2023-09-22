# Before import -----------------------------------------------------------


# Clear environment prior

rm(list = ls())

#yr = this year 20xx
yr <- 23
yr1 <- yr - 2
yr2 <-  yr - 3
#yr3 <-  yr - 4
# Load packages

library(tidyverse)
library(RtoSQLServer)
library(readxl)
library(haven)





#<-Datashare<-file path for import and export
Code_directory<-("//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Codeconversion_2023/2023")
sas_agstemp_path <-   "//s0177a/sasdata1/ags/census/agstemp/"
sas_agscens_path <-   "//s0177a/sasdata1/ags/census/agscens/"

# ADM schema for export

server <-   "s0196a\\ADM"
database <-   "RuralAndEnvironmentalScienceFarmingStatistics"
schema <-   "juneagriculturalsurvey2023alpha"



#Import-------------------------------------------------------------
source("June - E2- Adding CTS, LFA, SSC, zero areas and geographies.R")


#previous year's data (2021)

prev_yr <-  read_table_from_db(
  server = server,
  database = database,
  schema = schema,
  table_name = "jac_2021_update"
)


# Load functions

# Functions ---------------------------------------------------------------
recode_response <- function(x) {
  ifelse(x != 0 | !is.na(x), 1, 0)
}
recode_number_response <- function(x) {
  x %>%
    mutate(number_responses = rowSums(.))
}
# Summary -----------------------------------------------------------------

june_summary <-june_geog %>% select(where(is.numeric))
june_count <-as.data.frame(recode_response(june_summary)) 
june_summary <- as.data.frame(list(count = colSums(june_count, na.rm = TRUE), mean = colMeans(june_summary, na.rm = TRUE) ) ) 

rm(june_count)


# Checks ------------------------------------------------------------------

#livestock check
livestock <- june_geog %>% select(parish,
                                holding,
                                madeup,
                                saf_madeup,
                                ags_madeup,
                                item27780,
                                item94,
                                item145,
                                item157,
                                item170) %>% 
rename(goats="item27780",
       deer = "item94",
       sheep = "item145",
       pigs = "item157", 
       poultry= "item170")


cattle <- june_geog %>%
  filter(completedata !=1 & CTS312>0) %>% 
  select(starts_with("CTS")) %>% 
  mutate(year =paste0("20", yr))


cattle_prev <- rename_with(prev_yr, toupper) %>% 
  filter(COMPLETEDATA !=1 & CTS312>0) %>% 
  select(starts_with("CTS")) %>% 
  mutate(year =paste0("20", yr1))

cattle <- bind_rows(cattle, cattle_prev)
cattle$year <- as.numeric(cattle$year)
cattle_count <-as.data.frame(recode_response(cattle))   
cattle_summary <- as.data.frame(list(count = colSums(cattle_count, na.rm = TRUE), mean = colMeans(cattle, na.rm = TRUE) ) ) 

#20/9 - pause here: to do: all items data frame (sum and count) ...need to check all items object is relevant. might copy paste item list from main_validations  

rm()
# Export june_geog to ADM -------------------------------------------------

write_dataframe_to_db(
  server = server,
  database = database,
  schema = schema,
  table_name = "JAC23_preSOSLRTypology",
  dataframe = june_geog,
  append_to_existing = FALSE,
  versioned_table = FALSE,
  batch_size = 10000
)

#clear environment
rm(list = ls())


# Add Standard Outputs ----------------------------------------------------

June_SO <- june_geog %>% filter(completedata==1)

