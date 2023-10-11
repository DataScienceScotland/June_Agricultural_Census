#Script to produce CTS301-CTS312 using ScotEID data, imported in A1. 
# Before import -----------------------------------------------------------


# # Clear environment prior
# 
# rm(list = ls())

# Load packages

library(tidyverse)
library(RtoSQLServer)



# Load functions

# Functions ---------------------------------------------------------------


breed_add <- function(x){
  x %>% 
  rowwise() %>%
  mutate(n=sum(n.x, n.y, na.rm = TRUE)
  ) %>%
  select(-c(n.x, n.y))
  }

# Datashare file path for import and export

Code_directory <- ("//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Codeconversion_2023/2023")
sas_agstemp_path <- "//s0177a/sasdata1/ags/census/agstemp/"
sas_agscens_path <- "//s0177a/sasdata1/ags/census/agscens/"

# ADM schema for export

server <- "s0196a\\ADM"
database <- "RuralAndEnvironmentalScienceFarmingStatistics"
schema <- "juneagriculturalsurvey2023alpha"


#Import-------------------------------------------------------------
# From the ADM server 
#Import SCOTEID  data 
cattle <- read_table_from_db(server=server, 
                               database=database, 
                               schema=schema, 
                               table_name="scoteid_A_2023")


#change to post imputation dataset
June_data <- read_table_from_db(server=server, 
                                     database=database, 
                                     schema=schema, 
                                     table_name="post_imputation")


# Process variables -------------------------------------------------------

#create Parish and Holdings variable from "cph" variable
cattle$parish <- as.numeric(substr(cattle$cph, 4,6))
cattle$holding <- as.numeric(substr(cattle$cph, 8,11))

#remove birth_cph and cph to be able to group by parish and holding 
cattle <- cattle %>% select(-c(birth_cph,
                               cph,
                               current_wbm_cph),
                            -any_of("scoteid_A_2023ID"))
## NOTE Parish and Holding will only retain leading zeroes if remaining as strings, if format is changed to numeric then leading zeroes will be dropped
 

#set has_progeny as factor
cattle$has_progeny <- as.factor(cattle$has_progeny)
# Recode ------------------------------------------------------------------

#recode age-range as 6-12 and 12-18 imported as dates (06-Dec and Dec-18)
unique(cattle$age_range)
cattle$age_range <- recode_factor(cattle$age_range, 
                                "0-6" = "0-6", 
                                "06-Dec" = "6-12",
                                "Dec-18" = "12-18",
                                "18-24" = "18-24",
                                "24-30" = "24-30",
                                "30-36" = "30-36",
                                "36-48" = "36-48",
                                "48-60" = "48-60",
                                "60-72" = "60-72",
                                "72-84" = "72-84", 
                                ">84" = ">84",
                                "Unknown" = "Unknown")

# Add CTS age range -----------------------------------------------------------
#find out age-range values

cattle<- cattle %>%  mutate(cts_age_range = case_when(age_range %in% c("0-6", "6-12") ~ "u1",
                                                     age_range %in% c("12-18", "18-24")  ~ "12",
                                                     age_range %in% c("24-30", "30-36", "36-48", "48-60", "60-72", "72-84", ">84")~ "2",
                                                   TRUE ~"2"))



# Assign sex and age for unknowns -----------------------------------------
# 
# Assign sexless as male or female (depending on progeny)
cattle<- cattle %>%  mutate(sex = ifelse(!(sex %in% c("Male", "Female")), 
                                         ifelse(has_progeny == 0, "Male",
                                                ifelse(has_progeny == 1, "Female", as.character(sex))),
                                                as.character(sex)))



#Assign non dairy/ beef as dual 
cattle<- cattle %>%  mutate(produce = ifelse(!(produce %in% c("Beef", "Dairy", "Dual")), 
                                             "Dual", as.character(produce)))


# Create CTS category variable --------------------------------------------
cattle_original <- cattle

cattle <-cattle_original%>% 
  mutate(cts_category= (case_when (sex == "Female" & produce == "Beef" & cts_age_range == "u1" ~ "u1fbeef",
                                   sex == "Female" & produce == "Dairy"& cts_age_range == "u1" ~ "u1fdairy",
                                   sex == "Female" & produce == "Dual" & cts_age_range == "u1" ~ "u1fdual",
                                   sex == "Female" & produce == "Beef"& cts_age_range == "12" ~ "fbeef12",
                                   sex == "Female" & produce == "Dairy"& cts_age_range == "12" ~ "fdairy12",
                                   sex == "Female" & produce == "Dual" & cts_age_range == "12" ~ "fdual12",
                                   sex == "Female" & produce == "Beef" & cts_age_range == "2" & has_progeny == 0 ~ "fbeefnooff2",
                                   sex == "Female" & produce == "Beef" & cts_age_range == "2" & has_progeny == 1 ~ "fbeefwithoff2",
                                   sex == "Female" & produce == "Dairy"& cts_age_range == "2" & has_progeny == 0 ~ "fdairynooff2",
                                   sex == "Female" & produce == "Dairy"& cts_age_range == "2" & has_progeny == 1 ~ "fdairywithoff2",
                                   sex == "Female" & produce == "Dual" & cts_age_range == "2" & has_progeny == 0 ~ "fdualnooff2",
                                   sex == "Female" & produce == "Dual" & cts_age_range == "2" & has_progeny == 1 ~ "fdualwithoff2",
                                   sex == "Male" & cts_age_range == "u1" ~ "u1m",
                                   sex == "Male" & cts_age_range == "12" ~ "m12",
                                   sex == "Male" & cts_age_range == "2" ~ "m2",
                                   TRUE ~ "Unknown") ))%>%
 select(-c(breed, sex, produce, cts_age_range, has_progeny, age_range)) %>%
  group_by(parish, holding, cts_category)
# 
# 
#check
cattle %>% filter(cts_category == "Unknown")
# Totals check ------------------------------------------------------------
#Before portioning and manipulation

#male
male <- cattle_original %>% filter((cts_age_range== "u1"| cts_age_range== "12"| cts_age_range == "2" )& sex == "Male") 
male <- sum(male$cattle)
#female
female <- cattle_original %>% filter((cts_age_range== "u1"| cts_age_range== "12"| cts_age_range == "2" )& sex == "Female") 
female <- sum(female$cattle)

#female_unknown
female_unknown <- cattle_original %>% filter(cts_age_range== "Unknown" & sex == "Female") 
female_unknown <- sum(female_unknown$cattle)

#male and female
female_male_sum <- female + male

#no age or sex
no_age <- cattle_original %>% filter(cts_age_range=="Unknown")
no_age <- sum(no_age$cattle)
no_sex <- cattle_original %>% filter(cts_age_range!="Unknown" & sex !="Male" & sex != "Female")
no_sex <- sum(no_sex$cattle)

#check
total_cattle_start <- sum(cattle_original$cattle)
total_cattle_start - female_male_sum- no_age-no_sex

#duals
dual <- cattle_original %>% filter((cts_age_range== "u1"| 
                              cts_age_range== "12"|
                              cts_age_range == "2" ) &
                             sex == "Female" & 
                             produce == "Dual") 
dual <- sum(dual$cattle)
# Format data like CTS ----------------------------------------------------

#sum number of cattle in each  cts category for each parish holding 
cattle <- cattle %>%  dplyr::summarise(cattle = sum(cattle)) %>% as.data.frame()

#create names cts_category vector for later
cts_category <- unique(cattle$cts_category)

#pivot wider so cts categories are columns, fill missing data with 0
cattle <- cattle %>% ungroup() %>%
  pivot_wider(
  names_from = cts_category, 
  values_from = c(cattle), values_fill = 0)

#add "cattle" column to sum number of cattle for each parish holding
cattle$cattle <- rowSums(cattle[names(cattle) %in% cts_category])


# Original total ----------------------------------------------------------

total_cattle_start <- sum(cattle$cattle)
cattle$total_cattle <- sum(cattle$cattle)

# Duplicate check ---------------------------------------------------------

##check for duplicates
dup_cattle <- duplicated(cattle)
summary(dup_cattle)
dup_cattle<- cattle[duplicated(cattle), ]
summary(unique(cattle)==cattle)


# Remove Unknown -----------------------------------------------------------


#remove unknowns and blanks

#cattle <- cattle %>% select(-Unknown)

#sum without unknown
cattle$cattle <- rowSums(cattle[names(cattle) %in% cts_category])
total_cattle_no_unknown <- sum(cattle$cattle)



# Add total columns  ------------------------------------------------------

cattle$u1fbeeftotal <- sum(cattle$u1fbeef, na.rm=TRUE)
cattle$u1fdairytotal <- sum(cattle$u1fdairy, na.rm=TRUE)
cattle$fbeef12total <- sum(cattle$fbeef12, na.rm=TRUE)
cattle$fdairy12total <- sum(cattle$fdairy12, na.rm=TRUE)
cattle$fbeefnooff2total <- sum(cattle$fbeefnooff2, na.rm=TRUE)
cattle$fbeefwithoff2total <- sum(cattle$fbeefwithoff2, na.rm=TRUE)
cattle$fdairynooff2total <- sum(cattle$fdairynooff2, na.rm=TRUE)
cattle$fdairywithoff2total<- sum(cattle$fdairywithoff2, na.rm=TRUE)


# Dual portioning - Total level ---------------------------------------------------------

#proportion of each produce category out of total produce
cattle$u1fbeef_totalprop <- cattle$u1fbeeftotal/(cattle$u1fdairytotal+cattle$u1fbeeftotal)
cattle$u1fdairy_totalprop <- cattle$u1fdairytotal/(cattle$u1fdairytotal+cattle$u1fbeeftotal)
cattle$fbeef12_totalprop <- cattle$fbeef12total/(cattle$fdairy12total+cattle$fbeef12total)
cattle$fdairy12_totalprop <- cattle$fdairy12total/(cattle$fbeef12total+cattle$fdairy12total)
cattle$fbeefnooff2_totalprop <- cattle$fbeefnooff2total/(cattle$fdairynooff2total+cattle$fbeefnooff2total)
cattle$fdairynooff2_totalprop <- cattle$fdairynooff2total/(cattle$fdairynooff2total+cattle$fbeefnooff2total)
cattle$fbeefwithoff2_totalprop <- cattle$fbeefwithoff2total/(cattle$fdairywithoff2total+cattle$fbeefwithoff2total)
cattle$fdairywithoff2_totalprop <- cattle$fdairywithoff2total/(cattle$fdairywithoff2total+cattle$fbeefwithoff2total)

#portion of  dual according to portion of each produce category
cattle$u1fbeef_totalprop_dual <- cattle$u1fbeef_totalprop*cattle$u1fdual
cattle$u1fdairy_totalprop_dual <- cattle$u1fdairy_totalprop*cattle$u1fdual
cattle$fbeef12_totalprop_dual <- cattle$fbeef12_totalprop*cattle$fdual12
cattle$fdairy12_totalprop_dual <- cattle$fdairy12_totalprop*cattle$fdual12
cattle$fbeefnooff2_totalprop_dual <- cattle$fbeefnooff2_totalprop*cattle$fdualnooff2
cattle$fdairynooff2_totalprop_dual <- cattle$fdairynooff2_totalprop*cattle$fdualnooff2
cattle$fbeefwithoff2_totalprop_dual <- cattle$fbeefwithoff2_totalprop*cattle$fdualwithoff2
cattle$fdairywithoff2_totalprop_dual <- cattle$fdairywithoff2_totalprop*cattle$fdualwithoff2


#Dual portioning - Holding level -------------------------------------------------------------------------


#proportion of each produce category out of total produce
#fill NaN with 0

cattle$prop_u1fbeef <- cattle$u1fbeef/(cattle$u1fbeef+cattle$u1fdairy)
cattle[is.nan(cattle$prop_u1fbeef),"prop_u1fbeef"] <- 0

cattle$prop_u1fdairy <- cattle$u1fdairy/(cattle$u1fdairy+cattle$u1fbeef)
cattle[is.nan(cattle$prop_u1fdairy),"prop_u1fdairy"] <- 0

cattle$prop_fbeef12 <- cattle$fbeef12/(cattle$fbeef12+cattle$fdairy12)
cattle[is.nan(cattle$prop_fbeef12),"prop_fbeef12"] <- 0

cattle$prop_fdairy12 <- cattle$fdairy12/(cattle$fdairy12+cattle$fbeef12)
cattle[is.nan(cattle$prop_fdairy12),"prop_fdairy12"] <- 0

cattle$prop_fbeefnooff2 <- cattle$fbeefnooff2/(cattle$fbeefnooff2+cattle$fdairynooff2)
cattle[is.nan(cattle$prop_fbeefnooff2),"prop_fbeefnooff2"] <- 0

cattle$prop_fdairynooff2 <- cattle$fdairynooff2/(cattle$fdairynooff2+cattle$fbeefnooff2)
cattle[is.nan(cattle$prop_fdairynooff2),"prop_fdairynooff2"] <- 0

cattle$prop_fbeefwithoff2 <- cattle$fbeefwithoff2/(cattle$fbeefwithoff2+cattle$fdairywithoff2)
cattle[is.nan(cattle$prop_fbeefwithoff2),"prop_fbeefwithoff2"] <- 0

cattle$prop_fdairywithoff2 <- cattle$fdairywithoff2/(cattle$fdairywithoff2+cattle$fbeefwithoff2)
cattle[is.nan(cattle$prop_fdairywithoff2),"prop_fdairywithoff2"] <- 0

#portion of  dual according to portion of each produce category
cattle$prop_u1fbeef_dual <- cattle$u1fdual*cattle$prop_u1fbeef

cattle$prop_u1fdairy_dual <- cattle$u1fdual*cattle$prop_u1fdairy

cattle$prop_fbeef12_dual <- cattle$fdual12*cattle$prop_fbeef12

cattle$prop_fdairy12_dual <- cattle$fdual12*cattle$prop_fdairy12

cattle$prop_fbeefnooff2_dual <- cattle$fdualnooff2*cattle$prop_fbeefnooff2

cattle$prop_fdairynooff2_dual <- cattle$fdualnooff2*cattle$prop_fdairynooff2

cattle$prop_fbeefwithoff2_dual <- cattle$fdualwithoff2*cattle$prop_fbeefwithoff2

cattle$prop_fdairywithoff2_dual <- cattle$fdualwithoff2*cattle$prop_fdairywithoff2

cattle$prop_fdairywithoff2_dualnooff2 <- cattle$fdualnooff2*cattle$prop_fdairywithoff2

cattle$prop_fbeefwithoff2_dualnooff2 <- cattle$fdualnooff2*cattle$prop_fbeefwithoff2

cattle$prop_fdairynooff2_dualwithoff2 <- cattle$fdualwithoff2*cattle$prop_fdairynooff2

cattle$prop_fbeefnooff2_dualwithoff2 <- cattle$fdualwithoff2*cattle$prop_fbeefnooff2



## For holdings with dual cattle and only one of beef or dairy, move dual to beef or dairy

cattle[which(cattle$u1fbeef > 0 & cattle$u1fdairy == 0 & cattle$u1fdual > 0),"CTS301"] <- rowSums(cattle[which(cattle$u1fbeef > 0 & cattle$u1fdairy == 0 & cattle$u1fdual > 0),c("u1fbeef", "u1fdual" )], na.rm=TRUE)
cattle[which(cattle$u1fbeef > 0 & cattle$u1fdairy == 0 & cattle$u1fdual > 0),"u1fdual"] <- 0
cattle[which(cattle$u1fbeef == 0 & cattle$u1fdairy > 0 & cattle$u1fdual > 0),"CTS302"] <- rowSums(cattle[which(cattle$u1fbeef == 0 & cattle$u1fdairy > 0 & cattle$u1fdual > 0),c("u1fdairy", "u1fdual" )], na.rm=TRUE)
cattle[which(cattle$u1fbeef == 0 & cattle$u1fdairy > 0 & cattle$u1fdual > 0),"u1fdual"] <- 0


cattle[which(cattle$fbeef12 > 0 & cattle$fdairy12 == 0 & cattle$fdual12 > 0),"CTS303"] <- rowSums(cattle[which(cattle$fbeef12 > 0 & cattle$fdairy12 == 0 & cattle$fdual12 > 0),c("fbeef12", "fdual12" )], na.rm=TRUE)
cattle[which(cattle$fbeef12 > 0 & cattle$fdairy12 == 0 & cattle$fdual12 > 0),"fdual12"] <- 0
cattle[which(cattle$fbeef12 == 0 & cattle$fdairy12 > 0 & cattle$fdual12 > 0),"CTS304"] <- rowSums(cattle[which(cattle$fbeef12 == 0 & cattle$fdairy12 > 0 & cattle$fdual12 > 0),c("fdairy12", "fdual12" )], na.rm=TRUE)
cattle[which(cattle$fbeef12 == 0 & cattle$fdairy12 > 0 & cattle$fdual12 > 0),"fdual12"] <- 0


cattle[which(cattle$fbeefnooff2 > 0 & cattle$fdairynooff2 == 0 & cattle$fdualnooff2 > 0),"CTS307"] <- rowSums(cattle[which(cattle$fbeefnooff2 > 0 & cattle$fdairynooff2 == 0 & cattle$fdualnooff2 > 0),c("fbeefnooff2", "fdualnooff2" )], na.rm=TRUE)
cattle[which(cattle$fbeefnooff2 > 0 & cattle$fdairynooff2 == 0 & cattle$fdualnooff2 > 0),"fdualnooff2"] <- 0
cattle[which(cattle$fbeefnooff2 == 0 & cattle$fdairynooff2 > 0 & cattle$fdualnooff2 > 0),"CTS308"] <- rowSums(cattle[which(cattle$fbeefnooff2 == 0 & cattle$fdairynooff2 > 0 & cattle$fdualnooff2 > 0),c("fdairynooff2", "fdualnooff2" )], na.rm=TRUE)
cattle[which(cattle$fbeefnooff2 == 0 & cattle$fdairynooff2 > 0 & cattle$fdualnooff2 > 0),"fdualnooff2"] <- 0


cattle[which(cattle$fbeefwithoff2 > 0 & cattle$fdairywithoff2 == 0 & cattle$fdualwithoff2 > 0),"CTS305"] <- rowSums(cattle[which(cattle$fbeefwithoff2 > 0 & cattle$fdairywithoff2 == 0 & cattle$fdualwithoff2 > 0),c("fbeefwithoff2", "fdualwithoff2" )], na.rm=TRUE)
cattle[which(cattle$fbeefwithoff2 > 0 & cattle$fdairywithoff2 == 0 & cattle$fdualwithoff2 > 0),"fdualwithoff2"] <- 0
cattle[which(cattle$fbeefwithoff2 == 0 & cattle$fdairywithoff2 > 0 & cattle$fdualwithoff2 > 0),"CTS306"] <- rowSums(cattle[which(cattle$fbeefwithoff2 == 0 & cattle$fdairywithoff2 > 0 & cattle$fdualwithoff2 > 0),c("fdairywithoff2", "fdualwithoff2" )], na.rm=TRUE)
cattle[which(cattle$fbeefwithoff2 == 0 & cattle$fdairywithoff2 > 0 & cattle$fdualwithoff2 > 0),"fdualwithoff2"] <- 0

#
# ## For holdings with beef and dairy and dual
#
cattle[which(cattle$u1fbeef > 0 & cattle$u1fdairy > 0 & cattle$u1fdual > 0),"CTS301"] <- rowSums(cattle[which(cattle$u1fbeef > 0 & cattle$u1fdairy > 0 & cattle$u1fdual > 0),c("u1fbeef", "prop_u1fbeef_dual" )], na.rm=TRUE)
cattle[which(cattle$u1fbeef > 0 & cattle$u1fdairy > 0 & cattle$u1fdual > 0),"CTS302"] <- rowSums(cattle[which(cattle$u1fbeef > 0 & cattle$u1fdairy > 0 & cattle$u1fdual > 0),c("u1fdairy", "prop_u1fdairy_dual" )], na.rm=TRUE)
cattle[which(cattle$u1fbeef > 0 & cattle$u1fdairy > 0 & cattle$u1fdual > 0),"u1fdual"] <- 0


cattle[which(cattle$fbeef12 > 0 & cattle$fdairy12 > 0 & cattle$fdual12 > 0),"CTS303"] <- rowSums(cattle[which(cattle$fbeef12 > 0 & cattle$fdairy12 > 0 & cattle$fdual12 > 0),c("fbeef12", "prop_fbeef12_dual" )], na.rm=TRUE)
cattle[which(cattle$fbeef12 > 0 & cattle$fdairy12 > 0 & cattle$fdual12 > 0),"CTS304"] <- rowSums(cattle[which(cattle$fbeef12 > 0 & cattle$fdairy12 > 0 & cattle$fdual12 > 0),c("fdairy12", "prop_fdairy12_dual" )], na.rm=TRUE)
cattle[which(cattle$fbeef12 > 0 & cattle$fdairy12 > 0 & cattle$fdual12 > 0),"fdual12"] <- 0


cattle[which(cattle$fbeefnooff2 > 0 & cattle$fdairynooff2 > 0 & cattle$fdualnooff2 > 0),"CTS307"] <- rowSums(cattle[which(cattle$fbeefnooff2 > 0 & cattle$fdairynooff2 > 0 & cattle$fdualnooff2 > 0),c("fbeefnooff2", "prop_fbeefnooff2_dual" )], na.rm=TRUE)
cattle[which(cattle$fbeefnooff2 > 0 & cattle$fdairynooff2 > 0 & cattle$fdualnooff2 > 0),"CTS308"] <- rowSums(cattle[which(cattle$fbeefnooff2 > 0 & cattle$fdairynooff2 > 0 & cattle$fdualnooff2 > 0),c("fdairynooff2", "prop_fdairynooff2_dual" )], na.rm=TRUE)
cattle[which(cattle$fbeefnooff2 > 0 & cattle$fdairynooff2 > 0 & cattle$fdualnooff2 > 0),"fdualnooff2"] <- 0


cattle[which(cattle$fbeefwithoff2 > 0 & cattle$fdairywithoff2 > 0 & cattle$fdualwithoff2 > 0),"CTS305"] <- rowSums(cattle[which(cattle$fbeefwithoff2 > 0 & cattle$fdairywithoff2 > 0 & cattle$fdualwithoff2 > 0),c("fbeefwithoff2", "prop_fbeefwithoff2_dual" )], na.rm=TRUE)
cattle[which(cattle$fbeefwithoff2 > 0 & cattle$fdairywithoff2 > 0 & cattle$fdualwithoff2 > 0),"CTS306"] <- rowSums(cattle[which(cattle$fbeefwithoff2 > 0 & cattle$fdairywithoff2 > 0 & cattle$fdualwithoff2 > 0),c("fdairywithoff2", "prop_fdairywithoff2_dual" )], na.rm=TRUE)
cattle[which(cattle$fbeefwithoff2 > 0 & cattle$fdairywithoff2 > 0 & cattle$fdualwithoff2 > 0),"fdualwithoff2"] <- 0


## For holdings where there are dual but no matching beef or dairy, but there is either beef OR dairy of another age bracket

## Assume all dual are dairy
cattle[which(cattle$fdualnooff2 > 0 & cattle$fdairynooff2 == 0 & cattle$fbeefnooff2 == 0 & cattle$fdairywithoff2 > 0 & cattle$fbeefwithoff2 == 0),"CTS308"] <- cattle[which(cattle$fdualnooff2 > 0 & cattle$fdairynooff2 == 0 & cattle$fbeefnooff2 == 0 & cattle$fdairywithoff2 > 0 & cattle$fbeefwithoff2 == 0),"fdualnooff2"]
# Have a check - cattle[which(cattle$fdualnooff2 > 0 & cattle$fdairynooff2 == 0 & cattle$fbeefnooff2 == 0 & cattle$fdairywithoff2 > 0 & cattle$fbeefwithoff2 == 0),c("CTS308","fdualnooff2","fdairynooff2","fdairywithoff2")]
cattle[which(cattle$fdualnooff2 > 0 & cattle$fdairynooff2 == 0 & cattle$fbeefnooff2 == 0 & cattle$fdairywithoff2 > 0 & cattle$fbeefwithoff2 == 0),"fdualnooff2"] <- 0


## Assume all dual are beef
cattle[which(cattle$fdualnooff2 > 0 & cattle$fdairynooff2 == 0 & cattle$fbeefnooff2 == 0 & cattle$fdairywithoff2 == 0 & cattle$fbeefwithoff2 > 0),"CTS307"] <- cattle[which(cattle$fdualnooff2 > 0 & cattle$fdairynooff2 == 0 & cattle$fbeefnooff2 == 0 & cattle$fdairywithoff2 == 0 & cattle$fbeefwithoff2 > 0),"fdualnooff2"]
cattle[which(cattle$fdualnooff2 > 0 & cattle$fdairynooff2 == 0 & cattle$fbeefnooff2 == 0 & cattle$fdairywithoff2 == 0 & cattle$fbeefwithoff2 > 0),"fdualnooff2"] <- 0


## Split dualnooff2 according to dairy and beef over 2 proportions
cattle[which(cattle$fdualnooff2 > 0 & cattle$fdairynooff2 == 0 & cattle$fbeefnooff2 == 0 & cattle$fdairywithoff2 > 0 & cattle$fbeefwithoff2 > 0),"CTS308"] <- cattle[which(cattle$fdualnooff2 > 0 & cattle$fdairynooff2 == 0 & cattle$fbeefnooff2 == 0 & cattle$fdairywithoff2 > 0 & cattle$fbeefwithoff2 > 0),c("prop_fdairywithoff2_dualnooff2" )]
cattle[which(cattle$fdualnooff2 > 0 & cattle$fdairynooff2 == 0 & cattle$fbeefnooff2 == 0 & cattle$fdairywithoff2 > 0 & cattle$fbeefwithoff2 > 0),"CTS307"] <- cattle[which(cattle$fdualnooff2 > 0 & cattle$fdairynooff2 == 0 & cattle$fbeefnooff2 == 0 & cattle$fdairywithoff2 > 0 & cattle$fbeefwithoff2 > 0),c("prop_fbeefwithoff2_dualnooff2" )]
cattle[which(cattle$fdualnooff2 > 0 & cattle$fdairynooff2 == 0 & cattle$fbeefnooff2 == 0 & cattle$fdairywithoff2 > 0 & cattle$fbeefwithoff2 > 0),"fdualnooff2"] <- 0


## Assume all dual are dairy if there are dualwithoff2 and fdairynooff2
cattle[which(cattle$fdualwithoff2 > 0 & cattle$fdairywithoff2 == 0 & cattle$fbeefwithoff2 == 0 & cattle$fdairynooff2 > 0 & cattle$fbeefnooff2 == 0),"CTS306"] <- cattle[which(cattle$fdualwithoff2 > 0 & cattle$fdairywithoff2 == 0 & cattle$fbeefwithoff2 == 0 & cattle$fdairynooff2 > 0 & cattle$fbeefnooff2 == 0),"fdualwithoff2"]
cattle[which(cattle$fdualwithoff2 > 0 & cattle$fdairywithoff2 == 0 & cattle$fbeefwithoff2 == 0 & cattle$fdairynooff2 > 0 & cattle$fbeefnooff2 == 0),"fdualwithoff2"] <- 0


## Assume all dual are beef if there are dualwithoff2 and fbeefnooff2
cattle[which(cattle$fdualwithoff2 > 0 & cattle$fdairywithoff2 == 0 & cattle$fbeefwithoff2 == 0 & cattle$fdairynooff2 == 0 & cattle$fbeefnooff2 > 0),"CTS305"] <- cattle[which(cattle$fdualwithoff2 > 0 & cattle$fdairywithoff2 == 0 & cattle$fbeefwithoff2 == 0 & cattle$fdairynooff2 == 0 & cattle$fbeefnooff2 > 0),"fdualwithoff2"]
cattle[which(cattle$fdualwithoff2 > 0 & cattle$fdairywithoff2 == 0 & cattle$fbeefwithoff2 == 0 & cattle$fdairynooff2 == 0 & cattle$fbeefnooff2 > 0),"fdualwithoff2"] <- 0


## Split dualwithoff2 according to dairy and beef over 2 proportions
cattle[which(cattle$fdualwithoff2 > 0 & cattle$fdairywithoff2 == 0 & cattle$fbeefwithoff2 == 0 & cattle$fdairynooff2 > 0 & cattle$fbeefnooff2 > 0),"CTS306"] <- cattle[which(cattle$fdualwithoff2 > 0 & cattle$fdairywithoff2 == 0 & cattle$fbeefwithoff2 == 0 & cattle$fdairynooff2 > 0 & cattle$fbeefnooff2 > 0),c("prop_fdairynooff2_dualwithoff2" )]
cattle[which(cattle$fdualwithoff2 > 0 & cattle$fdairywithoff2 == 0 & cattle$fbeefwithoff2 == 0 & cattle$fdairynooff2 > 0 & cattle$fbeefnooff2 > 0),"CTS305"] <- cattle[which(cattle$fdualwithoff2 > 0 & cattle$fdairywithoff2 == 0 & cattle$fbeefwithoff2 == 0 & cattle$fdairynooff2 > 0 & cattle$fbeefnooff2 > 0),c("prop_fbeefnooff2_dualwithoff2" )]
cattle[which(cattle$fdualwithoff2 > 0 & cattle$fdairywithoff2 == 0 & cattle$fbeefwithoff2 == 0 & cattle$fdairynooff2 > 0 & cattle$fbeefnooff2 > 0),"fdualwithoff2"] <- 0


## Holdings that only have dual breeds

cattle[which(cattle$u1fdual > 0),"CTS301"] <- rowSums(cattle[which(cattle$u1fdual > 0),c("u1fbeef","u1fbeef_totalprop_dual")], na.rm=TRUE)
cattle[which(cattle$u1fdual > 0),"CTS302"] <- rowSums(cattle[which(cattle$u1fdual > 0),c("u1fdairy","u1fdairy_totalprop_dual")], na.rm=TRUE)

cattle[which(cattle$fdual12 > 0),"CTS303"] <- rowSums(cattle[which(cattle$fdual12 > 0),c("fbeef12","fbeef12_totalprop_dual")], na.rm=TRUE)
cattle[which(cattle$fdual12 > 0),"CTS304"] <- rowSums(cattle[which(cattle$fdual12 > 0),c("fdairy12","fdairy12_totalprop_dual")], na.rm=TRUE)

cattle[which(cattle$fdualwithoff2 > 0),"CTS305"] <- rowSums(cattle[which(cattle$fdualwithoff2 > 0),c("fbeefwithoff2","fbeefwithoff2_totalprop_dual")], na.rm=TRUE)
cattle[which(cattle$fdualwithoff2 > 0),"CTS306"] <- rowSums(cattle[which(cattle$fdualwithoff2 > 0),c("fdairywithoff2","fdairywithoff2_totalprop_dual")], na.rm=TRUE)

cattle[which(cattle$fdualnooff2 > 0),"CTS307"] <- rowSums(cattle[which(cattle$fdualnooff2 > 0),c("fbeefnooff2","fbeefnooff2_totalprop_dual")], na.rm=TRUE)
cattle[which(cattle$fdualnooff2 > 0),"CTS308"] <- rowSums(cattle[which(cattle$fdualnooff2 > 0),c("fdairynooff2","fdairynooff2_totalprop_dual")], na.rm=TRUE)


## Fill in for holdings with no dual cattle
#should be blank as NAs filled in code above
cattle[is.na(cattle$CTS301),"CTS301"] <- cattle[is.na(cattle$CTS301),"u1fbeef"]
cattle[is.na(cattle$CTS302),"CTS302"] <- cattle[is.na(cattle$CTS302),"u1fdairy"]
cattle[is.na(cattle$CTS303),"CTS303"] <- cattle[is.na(cattle$CTS303),"fbeef12"]
cattle[is.na(cattle$CTS304),"CTS304"] <- cattle[is.na(cattle$CTS304),"fdairy12"]
cattle[is.na(cattle$CTS305),"CTS305"] <- cattle[is.na(cattle$CTS305),"fbeefwithoff2"]
cattle[is.na(cattle$CTS306),"CTS306"] <- cattle[is.na(cattle$CTS306),"fdairywithoff2"]
cattle[is.na(cattle$CTS307),"CTS307"] <- cattle[is.na(cattle$CTS307),"fbeefnooff2"]
cattle[is.na(cattle$CTS308),"CTS308"] <- cattle[is.na(cattle$CTS308),"fdairynooff2"]

## Add male categories
cattle$CTS309 <- cattle$u1m
cattle$CTS310 <- cattle$m12
cattle$CTS311 <- cattle$m2


## Add total (CTS312)



## Check that total from before dual were divided out matches total afterwards

## Add total (CTS312)

cattle$CTS312 <- cattle$CTS301 + cattle$CTS302 +  cattle$CTS303 + cattle$CTS304 + cattle$CTS305 + cattle$CTS306 + cattle$CTS307 +
  cattle$CTS308 + cattle$CTS309+ cattle$CTS310 + cattle$CTS311

## Check that total from before dual were divided out matches total afterwards

sum(cattle$cattle) -sum(cattle$CTS312)

#check males
sum(cattle$CTS309)  +
  sum(cattle$CTS310) +
  sum(cattle$CTS311) ==
  male

#check females
sum(cattle$CTS301) +
  sum(cattle$CTS302) + 
  sum(cattle$CTS303) + 
  sum(cattle$CTS304) +
  sum(cattle$CTS305) + 
  sum(cattle$CTS306) + 
  sum(cattle$CTS307) +
  sum(cattle$CTS308) + female_unknown - female


# -------------------------------------------------------------------------


## Keep only june item numbers

cts <- cattle %>% select(parish, holding, CTS301, CTS302, CTS303, CTS304, CTS305, CTS306, CTS307,
                    CTS308, CTS309, CTS310, CTS311, CTS312)

#  VARIABLE NAMES AT FINISH
# 
# cts301 = female beef cattle under 1*/
#   /*cts302 = female dairy cattle under 1 */
#   /*cts303 = female beef cattle aged 1-2 */
#   /*cts304 = female dairy cattle aged 1-2 */
#   /*cts305 = female beef cattle with offspring aged 2 and over */
#   /*cts306 = female dairy cattle with offspring aged 2 and over */
#   /*cts307 = female beef cattle without offspring aged 2 and over */
#   /*cts308 = female dairy cattle without offspring aged 2 and over */
#   /*cts309 = male cattle under 1 */
#   /*cts310 = male cattle aged 1-2 */
#   /*cts311 = male cattle aged 2 and over */
#   /*cts312 = total cattle */


## Summary table

cts_summary <- colSums(cts[,c("CTS301","CTS302","CTS303","CTS304","CTS305","CTS306","CTS307","CTS308","CTS309","CTS310","CTS311","CTS312")])


## Round cattle numbers - portioning out "dual" leads to decimals that need rounding
## Need to round .5s so that totals remain the same

for (i in 1:nrow(cts)) {
  for (j in 3:13) {
    if ((cts[i,j] - as.integer(cts[i,j]) == 0.5)) {
      if (j<=10 & j%%2 == 1) {
        if (cts[i,j] > cts[i,j+1]) {
          cts[i,j] <- ceiling(cts[i,j])
          cts[i,j+1] <- floor(cts[i,j+1])
        } else {
          cts[i,j] <- floor(cts[i,j])
          cts[i,j+1] <- ceiling(cts[i,j+1])
        }
      }
    } else {
      cts[i,j] <- round(cts[i,j])
    }
  }
}

cts[,c("CTS301","CTS302","CTS303","CTS304","CTS305","CTS306","CTS307","CTS308","CTS309","CTS310","CTS311")] <- round(cts[,c("CTS301","CTS302","CTS303","CTS304","CTS305","CTS306","CTS307","CTS308","CTS309","CTS310","CTS311")] )

## Summary table after rounding

cts_summary_after_rounding <- colSums(cts[,c("CTS301","CTS302","CTS303","CTS304","CTS305","CTS306","CTS307","CTS308","CTS309","CTS310","CTS311","CTS312")])


## Publication table
## Assign latest year
latest_yr <- 2023

cts_time_series <- enframe(colSums(cts[,c("CTS301","CTS302","CTS303","CTS304","CTS305","CTS306","CTS307","CTS308","CTS309","CTS310","CTS311","CTS312")]), name="Item", value=paste("Year",2023,sep="_"))
cts_time_series$groups <- c("Calves","Calves","Female Beef Cattle","Female Dairy Cattle","Female Beef Cattle","Female Dairy Cattle","Female Beef Cattle",
                            "Female Dairy Cattle","Calves","Male Cattle","Male Cattle","Total Cattle")
cts_time_series$ages <- c("Female beef cattle under 1","Female dairy cattle under 1","Aged 1-2","Aged 1-2","Aged 2 years and over - with offspring",
                          "Aged 2 years and over - with offspring","Aged 2 years and over - without offspring","Aged 2 years and over - without offspring",
                          "Male cattle under 1","Aged 1-2","Aged 2 years and over","Total cattle")

# # Merge with JAC data -----------------------------------------------------
#  June_CTS <- full_join(June_data, cts, by = c("parish", "holding"))
# # # #Save Outputs----------------------------------------------------------------------------------------------
# # 
# # #JuneCTS
# write_dataframe_to_db(
#   server = server,
#   database = database,
#   schema = schema,
#   table_name = "JAC23_CTS",
#   dataframe = June_CTS,
#   append_to_existing = FALSE,
#   versioned_table = FALSE,
#   batch_size = 10000
# )

