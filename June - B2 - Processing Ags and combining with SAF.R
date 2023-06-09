##Code to combine Ags (SAF and Non-SAF aka full and partial AGS/JAC) and SAF datasets
## Based on sections B5 and B10 from the SAS project.
##NB June 2023 will only have one form - so no need to import and join df_SAF(ags) and df_Non_SAF to form the census (ags) dataset. It will just be imported as is....
##Data from 2023 - 9th June. First data extract from Ags.
##Written by Jackie Massaya 31/01/2023
##Modified by Lucy Nevard 08.06.23


rm(list = ls())

# Load packages
library(readxl)
library(tidyverse)
library(stringr)
library(janitor)
library(data.table)
library(skimr)
library(haven)
source("Functions/Functions.R")
source("Scripts/Item categories.R")
#change year here. 
yr <- 23
yr1 <- (yr - 2)  # this should normally be yr-1 but no census in 2022. 


output_path <-
  "//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Codeconversion_2023/2023/"
sas_agscens_path <- "//s0177a/sasdata1/ags/census/agscens/"


server <- "s0196a\\ADM"
database <- "RuralAndEnvironmentalScienceFarmingStatistics"
schema <- "juneagriculturalsurvey2023alpha"  # schema will change each year and need updating here. The table names can therefore stay the same 2024 onwards (will need editing below).

#Load the June Survey return dataset (output saved from A)
# Load from ADM

all_ags <- read_table_from_db(server=server, 
                              database=database, 
                              schema=schema, 
                              table_name="Ags_A_2023")

# Load from datashare
# load(paste0(output_path, "nonSAF_ags_", yr, ".rda"))

# Load SAF data previous modules- cleaned, combined formatted with census item numbers
# Load from ADM

all_saf <- read_table_from_db(server=server, 
                               database=database, 
                               schema=schema, 
                               table_name="allsaf_final_2023")


# Load fromdatashare
# all_saf<-loadRData(paste0(output_path, "allsaf_final.rda"))

####TO DO loop through lists, keep only items for nonSAF and SAF

##NB SAS code replaces missing numeric values with zeros, as imputation is for missing data only.

all_ags  <-
  all_ags %>% group_by(parish, holding) %>% arrange(by_group = TRUE)


all_ags <- all_ags %>% add_land_other() %>%
  mutate(survdata = as.factor("full"),
         i = 1 + length(c(all_glasshouse, "item85", "item86"))) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))  # if the last line doesn't work, restart the session



glasshouse_cols <- names(all_ags) %in% all_glasshouse
all_ags[glasshouse_cols] <-
  all_ags[glasshouse_cols] %>% mutate(across(where(is.numeric), ~ .x / 10000))


#split SAF into seasonal data and non-seasonal
SAF_SEAS_ONLY <- all_saf %>%
  filter(saf_land != 1) %>%
  select(parish, holding, mlc, brn, item2827, item2828, item2)


all_saf <- all_saf %>% filter (saf_land == 1)


#join all_SAF and all_ags
#staged process as need to add columns depending on if SAF and/ or ags data exists for holdings; various inner and anti joins of both datasets are combined.


SAF_JAC_inner <-
  inner_join(
    all_saf,
    all_ags,
    by = c('parish', 'holding'),
    all = TRUE,
    suffix = c(".SAF", ".ags")
  )

SAF_JAC_SAF_anti <-
  anti_join(
    all_saf,
    all_ags,
    by = c('parish', 'holding'),
    all = TRUE,
    suffix = c(".SAF", ".ags")
  )

SAF_JAC_JAC_anti <-
  anti_join(
    all_ags,
    all_saf,
    by = c('parish', 'holding'),
    all = TRUE,
    suffix = c(".SAF", ".ags")
  )




both_SAF_JAC_inner_full <- SAF_JAC_inner %>%
  filter(survdata == "full") %>%
  mutate(
    land_data = as.factor("both"),
    other_data = as.factor("yes"),
    saf_data = as.factor("complete"),
    imptype = as.factor("none")
  )



SAF_anti <- SAF_JAC_SAF_anti %>%
  mutate(
    survdata = as.factor("none"),
    land_data = as.factor("saf"),
    other_data = as.factor("no"),
    imptype = as.factor("AGS"),
    survtype = as.factor("SAF_only"),
    saf_data = as.factor("complete")
  )

JAC_anti_full <- SAF_JAC_JAC_anti %>%
  filter(survdata == "full") %>%
  mutate(
    land_data = as.factor("ags"),
    other_data = as.factor("yes"),
    saf_data = as.factor("none"),
    imptype = as.factor("none")
  )


inner_SAF_JAC <-
both_SAF_JAC_inner_full %>%
  #keep only SAF data for inner-join data
  select(!contains(".ags"))

#change names for binding with anti JAC and SAF.
names(inner_SAF_JAC) <- gsub(".SAF", "", names(inner_SAF_JAC))
#names(inner_SAF_JAC)
anti_JAC <- JAC_anti_full
full_JAC_SAF <-  bind_rows(inner_SAF_JAC, anti_JAC)
full_JAC_SAF <- bind_rows(full_JAC_SAF, SAF_anti)


  
#names(SAF_SEAS_ONLY)

#reorder columns
full_JAC_SAF <-
  full_JAC_SAF %>% select(
    parish,
    holding,
    survtype,
    imptype,
    survdata,
    land_data,
    saf_data,
    other_data,
    contains(all_items),
    contains(string_answers),
    mlc,
    brn,
    lfass_area,
    everything()
  )

full_JAC_SAF <-
  full_JAC_SAF %>% select(!c(submisType, maderight)) %>% arrange(by_group = TRUE)  # original script also took out item3166 and item2726 but these don't exist




#check if only SEASONAL SAF DATA exist and not in COMPLETE SAF + JAC data, add labels
full_JAC_SAF_SAF_SEAS_anti <-
  anti_join(SAF_SEAS_ONLY,
            full_JAC_SAF,
            by = c('parish', 'holding'),
            all = TRUE) %>%
  mutate(
    survdata = as.factor("none"),
    land_data = as.factor("none"),
    other_data = as.factor("no"),
    imptype = as.factor("full"),
    survtype = as.factor("None"),
    saf_data = as.factor("seasonal_only")
  )


#Add 'seasonal_only' to saf_data column of entries in both complete SAF+JAC and SEASONAL ONLY
full_JAC_SAF_SAF_SEAS_inner <-
  inner_join(
    SAF_SEAS_ONLY,
    full_JAC_SAF,
    by = c('parish', 'holding'),
    all = TRUE,
    suffix = c(".SAF_SEAS", "JAC_SAF")
  ) %>%
  mutate(saf_data = as.factor("seasonal_only")) %>%
  select(!contains("JAC_SAF")) %>% arrange(by_group = TRUE)

names(full_JAC_SAF_SAF_SEAS_inner) <-
  gsub(".SAF_SEAS", "", names(full_JAC_SAF_SAF_SEAS_inner))




FJS_SEAS <-
  bind_rows(full_JAC_SAF_SAF_SEAS_inner, full_JAC_SAF_SAF_SEAS_anti)

FJS_anti <-
  anti_join(full_JAC_SAF,
            FJS_SEAS,
            by = c('parish', 'holding'),
            all = TRUE)
FJS <- bind_rows(FJS_anti, FJS_SEAS) %>% arrange(by_group = TRUE)

#******QA **************DO MANUAL CORRECTIONS HERE??********************* QA****



full_JAC_SAF_corr <- FJS %>%
  group_by(parish, holding) %>% arrange(by_group = TRUE)


check_glasshouse <-
  full_JAC_SAF_corr %>% ungroup() %>% filter(land_data == "saf" &
                                 sum(item85, item86) > 0)


#add in new items
#dealing with NAs - remove from sum to get value. Remove "na.rm = TRUE" to return NA (otherwise get zeros, imputation for genuinely missing values not zeros)
full_JAC_SAF <-
  full_JAC_SAF_corr %>% mutate(
    #item87 = sum(item2713, item2707, na.rm = TRUE),
    #item2036 = sum(item2714, item2708),
    
    #item2037 = sum(item2715, item2709),
    
    
    #item1711 = sum(item2716, item2710),
    #item1943 = sum(item2717, item2711),
    
    item2556 = sum(item2858, item2863, na.rm = TRUE),
    
    item2557 = sum(item2859, item2864, na.rm = TRUE),
    
    item2836 = sum(item2860, item2865, na.rm = TRUE),
    
    #item6000 = sum(item2861, item2866, na.rm = TRUE),
    item6001 = sum(item2862, item2867),
    
    item68 = sum(
      item52,
      item53,
      item55,
      item56,
      item2323,
      item59,
      item60,
      item61,
      item63,
      item64,
      item65,
      item66,
      na.rm = TRUE
    ),
    item35 = item68,
    item38 = sum(
      item41,
      item85,
      item86,
      item2324,
      item1709,
      item80,
      item81,
      item1710,
      item82,
      item83,
      na.rm = TRUE
    ),
    item76 = sum(item70, item71, item72, item2832, item75, na.rm = TRUE),
    item37 = item76,
    item40 = sum(
      item14,
      item15,
      item16,
      item18,
      item17,
      item20,
      item22,
      item19,
      item23,
      item21,
      item24,
      item2320,
      item27,
      item28,
      item2034,
      item29,
      item30,
      item31,
      item2059,
      item32,
      item34,
      item35,
      item36,
      item76,
      item38,
      item2469,
      item2470,
      item3156,
      na.rm = TRUE
    ),
    
    item46 = sum(item40, item2321, item2322, na.rm = TRUE),
    item84 = sum(
      item2324,
      item1709,
      item80,
      item81,
      item1710,
      item82,
      item83,
      na.rm = TRUE
    )
  )




full_JAC_SAF <-
  full_JAC_SAF %>% mutate(
    item85 = case_when(
      survtype == "SAF" &
        land_data == "saf"  ~ sum(
          #item2713,
          item2858,
          item2859,
          item2860,
          item2861,
          #item2714,
          #item2715,
          #item2716,
          #item2717,
          item2862,
          na.rm = TRUE
        ),
      TRUE ~ as.numeric(item85)
    ),
    
    
    item86 = case_when(
      survtype == "SAF" &
        land_data == "saf"  ~ sum(
          item2707,
          item2863,
          item2864,
          item2865,
          #item2866,
          #item2708,
          #item2709,
          #item2710,
          #item2711,
          item2867,
          na.rm = TRUE
        ),
      TRUE ~
        as.numeric(item86)
    )
    
    
    
  )

full_JAC_SAF <-
  full_JAC_SAF %>% mutate(
    item2 = case_when(
      sum(item2827, item2828) > 0  ~ sum(item2827, item2828),
      TRUE ~ as.numeric(item2)
    ),
    item2879 =  case_when(sum(item2827, item2828) > 0  ~ 0,
                          TRUE ~ as.numeric(item2879))
  )

full_JAC_SAF <-
  full_JAC_SAF %>% mutate(
    item2 = case_when(item2879 > 0  ~ as.numeric(item2879),
                      TRUE ~ as.numeric(item2)),
    item2827 = case_when(item2879 > 0  ~ NA_real_,
                         TRUE ~ as.numeric(item2827)),
    item2828 = case_when(item2879 > 0 ~ NA_real_,
                         TRUE ~ as.numeric(item2828))
  )

full_JAC_SAF <-
  full_JAC_SAF %>% arrange(parish, holding, by_group = TRUE)

#clear unused dataframes
rm(
  anti_JAC,
  both_SAF_JAC_inner_full,
  both_SAF_JAC_inner_partial,
  full_JAC_SAF_SAF_SEAS_anti,
  full_JAC_SAF_SAF_SEAS_inner,
  inner_SAF_JAC,
  JAC_anti_full,
  JAC_anti_partial,
  SAF_anti,
  SAF_JAC_inner,
  SAF_JAC_JAC_anti,
  SAF_JAC_SAF_anti
)




#order columns according to SAS_combined


FJS <- full_JAC_SAF %>% select(-c(i, item185, item186))

FJS$survtype <- as.factor(FJS$survtype)
FJS$saf_data <- as.factor(FJS$saf_data)

# 
# 
# levels(FJS$survtype)[levels(FJS$survtype) == "SAF_only"] <-
#   "SAF only"
# levels(FJS$saf_data)[levels(FJS$saf_data) == "seasonal"] <-
#   "seasonal only"



FJS <- FJS %>%
  mutate(across(starts_with('item'), ~ as.numeric(.)))


FJS <- FJS %>%
  mutate(across(
    starts_with('item'),
    ~ ifelse(survtype != "SAF_only" & is.na(.), 0, .)
  ))



# FJS <- FJS %>% 
#   mutate(item46=(round(as.numeric(item46),3)),
#          item40=(round(as.numeric(item40),3)),
#          item2=(round(as.numeric(item2),3)),
#          item68=(round(as.numeric(item68),3)),
#          item35=(round(as.numeric(item35),3)),
#          item38=(round(as.numeric(item38),3)),
#          item37=(round(as.numeric(item37),3)),
#          item76=(round(as.numeric(item76),3)),
#          item84=(round(as.numeric(item84),3)),
#          item85=(round(as.numeric(item85),3)),
#          item86=(round(as.numeric(item86),3)),
#          item2037=(round(as.numeric(item2037),3)),
#          item2861=(round(as.numeric(item2861),3)),
#          item87=(round(as.numeric(item87),3)),
#          item2036=(round(as.numeric(item2036),3)),
#          item1711=(round(as.numeric(item1711),3)),
#          item6000=(round(as.numeric(item6000),3)),
#          item6001=(round(as.numeric(item6001),3)))
         



~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~
  ~  ~  ~ #additional checks~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#duplicates
#full_JAC_SAF %>% distinct() %>% nrow() # check and edit this - currently does nothing

#glasshouse check
# glasshouse_check <-
#   full_JAC_SAF %>% select(parish,
#                           holding,
#                           survtype,
#                           land_data,
#                           other_data,
#                           all_of(all_glasshouse)) %>%
#   mutate(sum_glasshouse = rowSums(across(contains(all_glasshouse)))) %>%  # check this - it returns error in all_of(all_glasshouse)
#   filter(sum_glasshouse > 0)

#returns check. Run these when census is closed, i.e. returns are completed.

# prev_yr_data <-
#   read_sas(paste0(sas_agscens_path, "june", yr1, ".sas7bdat"))
# prev_yr_data_ph <- prev_yr_data %>% select(parish, holding)
# returns <- full_JAC_SAF %>% select(parish, holding, other_data) %>%
#   inner_join(., prev_yr_data_ph, by = c("parish", "holding"))
# #returns <- inner_join(returns, prev_yr_data_ph, by = c("parish", "holding"))
# 
# returns_23 <-
#   inner_join(full_JAC_SAF, returns, by = c("parish", "holding", "other_data"))
# 
# #check if other_data cols are identical
# #identical(returns_21$other_data.x, returns_21$other_data.y)
# 
# data.table(returns_23)
# 
# returns_23_summary <- returns_23 %>% select(where(is.numeric))
# returns_23_sum <-
#   as.data.frame(colSums(returns_23_summary, na.rm = TRUE)) %>%
#   rename(Sum = 1)
# returns_23_count <-
#   returns_23_summary %>% group_by_all() %>% count()


# Merge in total area and total rented area from 1st June address file (RP&S). 

addressfileorig<-read_sas(paste0(sas_agscens_path, "address_occid_01jun", yr, ".sas7bdat"))

addressfile<-clean_names(addressfileorig)

addressfile<-addressfile %>% 
  select(c(parish, holding, tot_area, tot_own_area, totrented_area)) %>% 
  rename(rps_totarea_june="tot_area",
         rps_totowned_june = "tot_own_area",
         rps_totrented_june = "totrented_area") %>% 
  mutate(parish=as.integer(parish),
         holding=as.integer(holding))


FJSaddress<-left_join(FJS, addressfile, by=c("parish", "holding"))


FJScheck<-FJSaddress %>% 
  select(parish, holding, survtype, item20026, item11, item12, rps_totarea_june, rps_totowned_june, rps_totrented_june)

# Save to datashare

save(FJSaddress, file = paste0(output_path, "/combined_data_2023.rda"))

# Save to ADM server


write_dataframe_to_db(server=server,
                      database=database,
                      schema=schema,
                      table_name="combined_data_2023",
                      dataframe=FJSaddress,
                      append_to_existing = FALSE,
                      versioned_table=FALSE,
                      batch_size = 10000)
