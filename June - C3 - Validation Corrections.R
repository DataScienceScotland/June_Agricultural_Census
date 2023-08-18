
# June  - Validation Error Corrections ------------------------------------

#Script corrects errors from C2 validation checks  

library(tidyverse)
library(RtoSQLServer)
library(writexl)
library(janitor)

# Clear environment prior
rm(list = ls())

#yr = this year 20xx
yr <- 2023
yr1 <- yr - 1
yr2 <-  yr - 2
yr3 <-  yr - 3
yr_list <- c(yr, yr2, yr3)


#Functions -----------------------------------------------------------


#remove total errors per case and ID col before joining
remove_total_id <- function(x) { x <-  x %>% select(-total_errors_per_case, -contains("ID", ignore.case = FALSE))  }




# Before import -----------------------------------------------------------

source("./item_numbers.R")


# ADM schema for export
server <- "s0196a\\ADM"

database <- "RuralAndEnvironmentalScienceFarmingStatistics"
schema <- "juneagriculturalsurvey2023alpha"



# Import ------------------------------------------------------------------

#combined dataset (SAF and JAC data)


combined_JAC <- read_table_from_db(
  server = server,
  database = database,
  schema = schema,
  table_name = "combined_data_2023"
)

combined_JAC <- combined_JAC %>% group_by(parish, holding)

#croft dataset

croft <- read_table_from_db(
  server = server,
  database = database,
  schema = schema,
  table_name = "crofts_A_2023"
)

croft <- croft %>% group_by(parish, holding)

#main validation_lists

#main validations_high priority list
mv_priority <- read_table_from_db(
  server = server,
  database = database,
  schema = schema,
  table_name = "JAC23_main_validation_list"
)

#main validations lower priority list
mv_low_priority <- read_table_from_db(
  server = server,
  database = database,
  schema = schema,
  table_name = "JAC23_main_validation_non_priority_list_full"
)

#main validaiton summary
mv_summary <- read_table_from_db(
  server = server,
  database = database,
  schema = schema,
  table_name = "JAC23_main_validation_summary"
)

# Combine croft and combined dataset------------------------------------------------------------------

#filter for only form returns
all_JAC_form <- combined_JAC %>% group_by(parish, holding) %>%
  filter(survtype == "Non-SAF")
croft <- croft %>% mutate(croft = 1)

#form returns and croft dataset
all_JAC_form <-
  left_join(all_JAC_form, croft, by = c("parish", "holding")) %>%
  mutate(croft = case_when(croft == 1 ~ as.numeric(croft),
                           TRUE ~ 0))


# Combine main validation data frames --------------------------------------

#remove total errors per case and ID column before join 
mv_priority <- remove_total_id(mv_priority) 
mv_low_priority <- remove_total_id(mv_low_priority)

all_main_validations <- full_join(mv_priority, mv_low_priority, by = c("parish", "holding"))


# Join validations with main JAC dataset ----------------------------------

all_JAC_form <- full_join(all_JAC_form, all_main_validations, by = c("parish", "holding"))

# Err45 correction --------------------------------------------------------


#calculate and add column for total of land items
all_JAC_form <- all_JAC_form %>% mutate(
  total13b = 
    get(wheat) + 
    get(all_barley) +
    get(all_oats) +
    get(all_rape) +
    get(seed_potatoes) +
    get(ware_potatoes) +
    get(all_stockfeed) +
    get(all_veg_open) +
    get(all_fruit) +
    get(all_flow_bulb) +
    get(tot_open_plastic) +
    get(tot_solid_glass) +
    get(other_crops) +
    get(fland_lte5) +
    get(fland_gt5) +
    get(grass_lt5) +
    get(grass_gte5) +
    get(rough_graze) +
    get(woodland) +
    get(other_land)
  
)


#dataframe of holdings with err45
err45 <- all_JAC_form %>% filter(err45==1) %>% 
  select(any_of(c(total_area, total_land, rps_total_area)), total13b)

#if total land (item50) is zero, but total land use items (total13b) is greater than 0, replace item50 with total13b
all_JAC_form <- all_JAC_form %>% mutate(
  item50 = case_when(item50 ==0 & total13b !=0 ~ total13b,
                     TRUE ~ as.numeric(item50)
                     )
)

# Err46 correction --------------------------------------------------------

#dataframe of holdings with err46
err46<- all_JAC_form %>% filter(err46  == 1) %>% 
  select(any_of(c(total_area, total_land, rps_total_area)), total13b)


#check (should have been corrected in err45 above): if total land = 0 and total13b>0 
x <- err46 %>% filter(get(total_land) ==0 & total13b!=0) 
x_total_holdings <- nrow(x)

#remaining err46 holdings
x <- err46 %>% filter(abs(get(total_area) - get(total_land)) > 3) 
x_total_holdings <- nrow(x)

#change item50 to item12 if item50 is zero, total13b (sum of all land items) is zero AND if item12 ==rps_tot_area
all_JAC_form <- all_JAC_form %>% mutate(
  item50 = case_when(abs(item12 - item50) > 3 &
                       item50 ==0 &
                       item12 ==rps_totarea_june ~ item12,
                     TRUE ~ as.numeric(item50)
  )
)


#check (recreate err46 first)
x <- err46 %>% filter(item50==0 & total13b==0)

# Err32 correction --------------------------------------------------------

#calculate and add column for total labour
all_JAC_form <- all_JAC_form %>% mutate(
  total12=
      get(labour_ft_m_bp) +
      get(labour_ft_m_fam) +
      get(labour_ft_m_hired) +
      get(labour_ft_f_bp) +
      get(labour_ft_f_fam) +
      get(labour_ft_f_hired) +
      get(labour_pt_m_bp) +
      get(labour_pt_f_bp) +
      get(labour_pt_m_hired) +
      get(labour_pt_m_fam) +
      get(labour_pt_f_hired) +
      get(labour_pt_f_fam) +
      get(labour_cas_m) +
      get(labour_cas_f)
    )


#dataframe of holdings with err32
err32 <- all_JAC_form %>% filter(err32 ==1) %>% 
  select(any_of(total_labour), total12)

#change total labour(item200) to total of labour items (total12) if item200<total12 

all_JAC_form <- all_JAC_form %>% mutate(
  item200 = case_when(item200<total12 ~ total12,
                     TRUE ~ as.numeric(item200)
  )
)

#check (recreate err32 first)
x <- err32 %>% filter(total12==0 & item200 !=0)




# Err11 correction --------------------------------------------------------

#calculate and add columns for glasshouse totals
all_JAC_form <- all_JAC_form %>% mutate(
  total5b= get(all_glass_open) +
    get(glass_unused_open),
  total6b= get(all_glass_solid) +
    get(glass_unused_solid)
)


#dataframe of holdings with err11
err11 <- all_JAC_form %>% filter(err11 ==1) %>% 
  select(all_of(c(tot_open_plastic, tot_solid_glass)),
         total5b, total6b)

#change glasshouse totals (open soil = item85, solid floor = item86) to sum of glasshouse items (total5b/ total6b)

all_JAC_form <- all_JAC_form %>% 
  mutate(item85 = case_when(item85<total5b ~ total5b,
                            TRUE~ as.numeric(item85)
                            ),
         item86 = case_when(item86<total6b~ total6b,
                            TRUE~ as.numeric(item86)
         )
  )


#check (recreate err11 first)
x <- err11 %>% filter(item85<total5b| item86<total6b)


# Err13 correction --------------------------------------------------------

#calculate and add column for pig total
all_JAC_form <- all_JAC_form %>% mutate(
  total10= 
    get(sows_pig) + 
    get(gilts_pig) +
    get(other_sows_pig) +
    get(barren_sows_pig) +
    get(gilts_50_pig) +
    get(boars_pig) +
    get(fat_pig) +
    get(wean_pig) +
    get(piglet)
  )
#dataframe of holdings with err13
err13 <- all_JAC_form %>%  filter(err13 ==1) %>% 
  select(any_of(total_pig), total10)








