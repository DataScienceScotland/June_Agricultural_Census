##Code to combine Ags (SAF and Non-SAF aka full and partial AGS/JAC) and SAF datasets
## Based on sections B5 and B10 from the SAS project.
##NB June 2023 will only have one form - so no need to import and join df_SAF(ags) and df_Non_SAF to form the census (ags) dataset. It will just be imported as is....
##Data from 2023 - 26/09/23
##Written by Jackie Massaya 31/01/2023
##Modified by Lucy Nevard 26.09.23


rm(list = ls())

# Before Import -----------------------------------------------------------

# Load packages
library(readxl)
library(tidyverse)
library(stringr)
library(janitor)
library(data.table)
library(skimr)
library(haven)
library(RtoSQLServer)
source("Functions/Functions.R")
source("item_numbers.R")
#change year here. 
yr <- 23
yr1 <- (yr - 2)  # this should normally be yr-1 but no census in 2022. 


output_path <-
  "//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Codeconversion_2023/2023/"
sas_agscens_path <- "//s0177a/sasdata1/ags/census/agscens/"


server <- "s0196a\\ADM"
database <- "RuralAndEnvironmentalScienceFarmingStatistics"
schema <- "juneagriculturalsurvey2023alpha"  # schema will change each year and need updating here. The table names can therefore stay the same 2024 onwards (will need editing below).


# Import ------------------------------------------------------------------


#Load the June Survey return dataset (output saved from A)
# Load from ADM

all_ags <- read_table_from_db(server=server, 
                              database=database, 
                              schema=schema, 
                              table_name="Ags_A_2023")
all_ags <- all_ags %>% 
  select(-any_of("Ags_A_2023ID"))

# Load from datashare
# load(paste0(output_path, "nonSAF_ags_", yr, ".rda"))

# Load SAF data previous modules- cleaned, combined formatted with census item numbers
# Load from ADM

all_saf <- read_table_from_db(server=server, 
                               database=database, 
                               schema=schema, 
                               table_name="allsaf_final_2023")
all_saf <- all_saf %>% 
  select(-any_of("allsaf_final_2023ID"))


june2015 <- read_table_from_db(server=server, 
                              database=database, 
                              schema=schema, 
                              table_name="june2015_robust_new")

unmatched<-read_table_from_db(server=server, 
                              database=database, 
                              schema=schema, 
                              table_name="unmatched_EXCL_SAF_B1")

# Load fromdatashare
# all_saf<-loadRData(paste0(output_path, "allsaf_final.rda"))

# Process Ags -------------------------------------------------------------


####TO DO loop through lists, keep only items for nonSAF and SAF

##NB SAS code replaces missing numeric values with zeros, as imputation is for missing data only.

all_ags  <-
  all_ags %>% arrange(parish, holding)


all_ags <- all_ags %>% add_land_other() %>%
  mutate(survdata = as.factor("full"),
         i = 1 + length(c(all_glasshouse, "item85", "item86"))) %>%
  dplyr::mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .))) 

# Convert glasshouse from sq m to hectares

all_ags <- all_ags %>% mutate(across(any_of(all_glasshouse), ~ .x / 10000))

# Process SAF: split into seasonal/non-seasonal ---------------------------


#split SAF into seasonal data and non-seasonal
SAF_SEAS_ONLY <- all_saf %>%
  filter(saf_land != 1) %>%
  select(parish, holding, mlc, brn, item2827, item2828, item2)


all_saf <- all_saf %>% filter (saf_land == 1)

# join all_saf and all_ags-------------------------------------------------------------------------
#staged process as need to add columns depending on if SAF and/ or ags data exists for holdings; various inner and anti joins of both datasets are combined.


SAF_JAC_inner <-
  inner_join(
    all_saf,
    all_ags,
    by = c('parish', 'holding'),
    all = TRUE,
    suffix = c(".SAF", ".ags")
  )


check<-SAF_JAC_inner %>% 
  select(parish, holding, item2321.SAF, item2321.ags, item2322.SAF, item2322.ags)



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
    submisType,
    survtype,
    imptype,
    survdata,
    land_data,
    saf_data,
    other_data,
    any_of(all_items),
    any_of(string_answers),
    mlc,
    brn,
    lfass_area,
    everything()
  )

full_JAC_SAF <-
  full_JAC_SAF %>% select(!maderight) %>% dplyr::arrange(.by_group = TRUE)  # original script also took out item3166 and item2726 but these don't exist




# Add SAF seasonal data -------------------------------------------------------


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
  select(!contains("JAC_SAF")) %>% dplyr::arrange(.by_group = TRUE)

names(full_JAC_SAF_SAF_SEAS_inner) <-
  gsub(".SAF_SEAS", "", names(full_JAC_SAF_SAF_SEAS_inner))




FJS_SEAS <-
  bind_rows(full_JAC_SAF_SAF_SEAS_inner, full_JAC_SAF_SAF_SEAS_anti)

FJS_anti <-
  anti_join(full_JAC_SAF,
            FJS_SEAS,
            by = c('parish', 'holding'),
            all = TRUE)
FJS <- bind_rows(FJS_anti, FJS_SEAS) %>% dplyr::arrange(.by_group = TRUE)



full_JAC_SAF_corr <- FJS %>%
  group_by(parish, holding) %>% dplyr::arrange(.by_group = TRUE)


check_glasshouse <-
  full_JAC_SAF_corr %>% ungroup() %>% filter(land_data == "saf" &
                                 sum(item85, item86) > 0)


# Add new items to combined dataset ---------------------------------------


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
    item6001 = sum(item2862, item2867, na.rm=TRUE),
    
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
      #item22,
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
      survtype == "SAF_only" &
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
      survtype == "SAF_only" &
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
  full_JAC_SAF %>% dplyr::arrange(parish, holding, .by_group = TRUE)

#clear unused dataframes
rm(
  anti_JAC,
  both_SAF_JAC_inner_full,
  full_JAC_SAF_SAF_SEAS_anti,
  full_JAC_SAF_SAF_SEAS_inner,
  inner_SAF_JAC,
  JAC_anti_full,
  SAF_anti,
  SAF_JAC_inner,
  SAF_JAC_JAC_anti,
  SAF_JAC_SAF_anti
)




#order columns according to SAS_combined
FJS<-full_JAC_SAF

FJS$survtype <- as.factor(FJS$survtype)
FJS$saf_data <- as.factor(FJS$saf_data)

# 
# 
# levels(FJS$survtype)[levels(FJS$survtype) == "SAF_only"] <-
#   "SAF only"
# levels(FJS$saf_data)[levels(FJS$saf_data) == "seasonal"] <-
#   "seasonal only"




FJS <- FJS %>%
  dplyr::mutate(across(starts_with('item') & !starts_with("item185") &!starts_with("item186"), ~ as.numeric(.))) 


FJS <- FJS %>%
  dplyr::mutate(across(
    starts_with('item')& !starts_with("item185") &!starts_with("item186"),
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
         




#additional checks~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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



# EXCL Fix part 2 ---------------------------------------------------------

# Part 1 of EXCL fix is in B1 (between "B8" and "B9")

strata23<-june2015 %>% 
  mutate(
    item48_15=item48,
    item49_15=item49,
    woodonly=
      ifelse(item48_15>0 & item49_15<=0, 1,""),
    othonly=
      ifelse(item48_15<=0 & item49_15>0, 1,""),
    noexcl=
      ifelse((item48_15+item49_15)<=0, 1, ""),
    wood_prop=
      ifelse(item48_15>0 | item49_15>0, (item48_15/(item48_15+ item49_15)), ""),
    strata=
      ifelse(sumso>=0 & sumso<10000, (robust_new-1)*6+1,
             ifelse(sumso>=10000 & sumso<20000, (robust_new-1)*6+2,
                    ifelse(sumso>=20000 & sumso<100000, (robust_new-1)*6+3,
                           ifelse(sumso>=100000 & sumso<250000, (robust_new-1)*6+4,
                                  ifelse(sumso>=250000 & sumso<500000, (robust_new-1)*6+5,
                                         ifelse(sumso>=500000, (robust_new-1)*6+6, 99)))))),
    strata=
      ifelse(is.na(strata), 99, strata)
  ) %>% 
  select(parish, holding, item48_15, item49_15, woodonly, othonly, noexcl, wood_prop, strata)

strata23_<-strata23 %>% 
  group_by(strata) %>% 
  dplyr::summarise(item48_15=sum(item48_15, na.rm=TRUE), item49_15=sum(item49_15, na.rm=TRUE))


strata_prop <- strata23_ %>% 
  mutate(wood_strata_prop = item48_15/(item48_15+ item49_15)) %>% 
  select(-item48_15,-item49_15)


strata23<-left_join(strata23, strata_prop, by="strata")

strata23$strata23_prop<-1

combined_with_strata<-left_join(FJS, strata23, by= c("parish", "holding"))



unmatchsplit<- unmatched %>% 
  filter((woodland15+otherland15) < newarea)


woodothadd <-unmatchsplit %>% 
  group_by(parish, holding) %>% 
  dplyr::summarise(woodland15=sum(woodland15),otherland15=sum(otherland15)) %>% 
  select(parish, holding, woodland15, otherland15)


combined15strata<-left_join(combined_with_strata, woodothadd, by=c("parish", "holding"))

combined15strata<-combined15strata %>% 
  mutate(
    item48=
      ifelse(woodland15>0, item48+woodland15,item48),
    item9999=
      ifelse(woodland15>0, item9999-woodland15, item9999),
    item49=
      ifelse(otherland15>0, item49+otherland15, item49),
    item9999=
      ifelse(otherland15>0, item9999-otherland15, item9999)
  )

combined15strata <- ungroup(combined15strata)

combined_data_excl<-combined15strata %>% 
  dplyr::mutate(
    wood_prop=as.numeric(wood_prop),
    item48_23=item48,
    item49_23=item49,
    wood_prop=
      ifelse((item48_15+item49_15)<=0, wood_strata_prop, wood_prop),
    wood_prop=
      ifelse(strata<=0, 0.746705, wood_prop),
    wood9999=(item9999*wood_prop)) 

combined_data_excl<-combined_data_excl %>% 
  mutate(
    flag=
      ifelse(item9999>0 & (item48+item49)<=0 & (item48_15+item49_15)<=0, 1,
             ifelse(item9999>0 & (item48+item49)<=0 & (item48_15+item49_15)>0, 2,
                    ifelse(item9999>0 & (item48>0 & item49 <=0) & (item48_15+item49_15)>0 & (item48_15>0 & item49_15<=0), 3,
                           ifelse(item9999>0 & (item48>0 & item49 <=0) & (item48_15<=0 & item49_15>0), 4,
                                  ifelse(item9999>0 & (item48>0 & item49 <=0) & (item48_15+item49_15)<=0, 5,
                                         ifelse(item9999>0 & (item48<=0 & item49>0) & (item48_15+item49_15)>0 & (item48_15<=0 & item49_15>0), 6,
                                                ifelse(item9999>0 & (item48<=0 & item49>0) & (item48_15+item49_15)>0 & (item48_15>0 & item49_15<=0), 7,
                                                       ifelse(item9999>0 & (item48<=0 & item49>0) & (item48_15+item49_15)<=0, 8,
                                                              ifelse(item9999>0 & (item48>0 & item49>0) & (item48_15+item49_15)>0, 9,
                                                                     ifelse(item9999>0 & (item48>0 & item49>0) & (item48_15+item49_15)<=0, 10, "NA")))))))))),
    item48=
      ifelse(item9999>0 & (item48_23+item49_23)<=0 & (item48_15+item49_15)<=0, wood9999, 
             ifelse(item9999>0 & (item48_23+item49_23)<=0 & (item48_15+item49_15)>0, wood9999,
                           ifelse(item9999>0 & (item48_23>0 & item49_23 <=0) & (item48_15+item49_15)>0 & (item48_15>0 & item49_15<=0), (item48_23+item9999),
                                  ifelse(item9999>0 & (item48_23>0 & item49_23 <=0) & (item48_15+item49_15)>0 & (item48_15>0 & item49_15>0), (item48_23+wood9999),
                                         ifelse(item9999>0 & (item48_23>0 & item49_23 <=0) & (item48_15+item49_15)<=0, (item48_23+wood9999),
                                                ifelse(item9999>0 & (item48_23<=0 & item49_23>0) & (item48_15+item49_15)>0 & (item48_15>0 & item49_15>0), wood9999,
                                                       ifelse(item9999>0 & (item48_23<=0 & item49_23>0) & (item48_15+item49_15)>0 & (item48_15>0 & item49_15<=0), item9999,
                                                              ifelse(item9999>0 & (item48_23<=0 & item49_23>0) & (item48_15+item49_15)<=0, wood9999,
                                                                     ifelse(item9999>0 & (item48_23>0 & item49_23>0) & (item48_15+item49_15)>0, item48_23+wood9999, 
                                                                            ifelse(item9999>0 & (item48_23>0 & item49_23>0) & (item48_15+item49_15)<=0, item48_23+wood9999, item48)))))))))),
    item49=
      ifelse(item9999>0 & (item48_23+item49_23)<=0 & (item48_15+item49_15)<=0, (item9999-wood9999),
             ifelse(item9999>0 & (item48_23+item49_23)<=0 & (item48_15+item49_15)>0, (item9999-wood9999),
                    ifelse(item9999>0 & (item48_23>0 & item49_23 <=0) & (item48_15+item49_15)>0 & (item48_15>0 & item49_15>0), (item9999-wood9999),
                           ifelse(item9999>0 & (item48_23>0 & item49_23 <=0) & (item48_15+item49_15)>0 & (item48_15>0 & item49_15>0), (item9999-wood9999),
                                  ifelse(item9999>0 & (item48_23>0 & item49_23 <=0) & (item48_15<=0 & item49_15>0), item9999,
                                         ifelse(item9999>0 & (item48_23>0 & item49_23 <=0) & (item48_15+item49_15)<=0, (item9999-wood9999),
                                                ifelse(item9999>0 & (item48_23<=0 & item49_23>0) & (item48_15+item49_15)>0 & (item48_15<=0 & item49_15>0), (item49_23+item9999),
                                                       ifelse(item9999>0 & (item48_23<=0 & item49_23>0) & (item48_15+item49_15)>0 & (item48_15>0 & item49_15>0), (item49_23+(item9999-wood9999)),
                                                              ifelse(item9999>0 & (item48_23<=0 & item49_23>0) & (item48_15+item49_15)<=0, (item49_23+(item9999-item48_23)),
                                                                     ifelse(item9999>0 & (item48_23>0 & item49_23>0) & (item48_15+item49_15)>0, item49+(item9999-wood9999),
                                                                            ifelse(item9999>0 & (item48_23>0 & item49_23>0) & (item48_15+item49_15)<=0, item49_23+(item9999-wood9999), item49))))))))))),
    item48=
      ifelse(item9999>0 & item48<0.1, 0, item48),
    item49=
      ifelse(item9999>0 & item48<0.1, item48+item49, item49),
    item48=
      ifelse(is.na(item48), 0, item48),
    item49=
      ifelse(is.na(item49), 0, item49)

  )
                                      
  
    
combined_data_2023<-combined_data_excl %>% 
  rowwise() %>% 
  mutate(
  item50=ifelse(saf_data!="none", sum(c(item46,item47,item48,item49), na.rm=TRUE), item50))

check<-combined_data_2023 %>% 
  select(parish, holding, item46, item47, item48, item49, item48_15, item49_15, strata, wood9999, item9999,  wood_prop, item48_23, item49_23, flag, item50, survtype, land_data, saf_data)

checkorig<-combined_with_strata %>% 
  select(parish, holding, item48, item49, item48_15, item49_15, strata, item9999,  wood_prop, item50, survtype, saf_data)


# Add additional checks here from SAS project. 
   
# Add RP&S address file areas ----------------------------------------------


# Merge in total area and total rented area from 1st June address file (RP&S). 


addressfileorig<-read_table_from_db(server=server, 
                                    database=database, 
                                    schema=schema, 
                                    table_name="address_occid_01jun_2023")

addressfileorig<-clean_names(addressfileorig)

addressfile<-addressfileorig %>% 
  select(c(parish, holding, tot_area, tot_own_area, totrented_area)) %>% 
  dplyr::rename(rps_totarea_june="tot_area",
         rps_totowned_june = "tot_own_area",
         rps_totrented_june = "totrented_area") %>% 
  mutate(parish=as.integer(parish),
         holding=as.integer(holding))


combined_data_address<-left_join(combined_data_2023, addressfile, by=c("parish", "holding"))


check<-combined_data_address %>% 
  select(parish, holding, survtype, item20026, item11, item12, rps_totarea_june, rps_totowned_june, rps_totrented_june)

combined_data_address<-data.frame(combined_data_address)

combined_data_address <- combined_data_address %>% 
  mutate(across(where(is.numeric), round_half_up, 2))



combined_data_address<-data.frame(combined_data_address)

combined_data_address[combined_data_address== ""] <- NA

# Save outputs ------------------------------------------------------------



# Save to datashare

save(combined_data_address, file = paste0(output_path, "/combined_data_2023_with_excl_variables.rda"))

# Save to ADM server

combined_data_address_red<-combined_data_address %>% 
  select(!(item48_15:wood9999))

save(combined_data_address_red, file = paste0(output_path, "/combined_data_2023.rda"))

write_dataframe_to_db(server=server,
                      database=database,
                      schema=schema,
                      table_name="combined_data_2023",
                      dataframe=combined_data_address_red,
                      append_to_existing = FALSE,
                      versioned_table=FALSE,
                      batch_size = 10000)

