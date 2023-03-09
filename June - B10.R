##Code to combine Ags (SAF and Non-SAF aka full and parital AGS/JAC) and SAF datasets
##NB June 2023 will only have one form - so no need to import and join df_SAF(ags) and df_Non_SAF to form the census (ags) dataset. It will just be imported as is....
##Written by Jackie Massaya 31/01/2023


rm(list=ls())

# Load packages
library(readxl)
library(tidyverse)
library(stringr)
library(janitor)
library(data.table)
library(skimr)
source("Functions/B10 Functions.R")
source("Scripts/Item categories.R")
yr <- 21

#Load the June Survey return (SAFags (df_SAF) and Non-SAFags (df_nonSAF)) datasets (outputs saved from A2)

output_path <- "//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Codeconversion_2023/"

load(paste0(output_path, "SAF_ags_", yr,".rda"))
load(paste0(output_path, "nonSAF_ags_", yr,".rda"))

#Load SAF data previous modules- cleaned, combined formatted with census item numbers
all_SAF <- read.csv(paste0(output_path, "CENS_WIDE21.csv")) %>% group_by(parish, holding)


####TO DO loop through lists, keep only items for nonSAF and SAF

##NB SAS code replaces missing numeric values with zeros, as imputation is for missing data only. 

Non_SAF  <-  df_nonSAF %>% group_by(parish, holding) %>% arrange(by_group = TRUE)
Non_SAF <- Non_SAF %>% add_land_other() %>%
  mutate(survdata = as.factor("full"),
         i = 1 + length(c(all_glasshouse, "item85", "item86"))) %>% 
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))

glasshouse_cols <- names(Non_SAF) %in% all_glasshouse
Non_SAF[glasshouse_cols] <- Non_SAF[glasshouse_cols] %>% mutate(across(where(is.numeric), ~ .x / 10000))

SAF_ags <- df_SAF %>% group_by(parish, holding) %>% arrange(by_group = TRUE)
SAF_ags <-  SAF_ags%>% add_land_other() %>%
  mutate(survdata = as.factor("part"),
         i = 1 + length(c(all_glasshouse, "item85", "item86")))%>% 
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))

glasshouse_cols <- names(SAF_ags) %in% all_glasshouse
SAF_ags[glasshouse_cols] <- SAF_ags[glasshouse_cols] %>% mutate(across(where(is.numeric), ~ .x / 10000))  

rm(df_SAF, df_nonSAF)

#combine Non_SAF and SAF_ags into one df - the full ags census dataset
##for June 2023, no need to do this, as will just be one dataset from the start. 
all_ags <- bind_rows(Non_SAF, SAF_ags)


rm(Non_SAF, SAF_ags)

#split SAF into seasonal data and non-seasonal
SAF_SEAS_ONLY <- all_SAF %>% 
  filter(saf_land !=1) %>% 
  select(parish, holding, mlc, brn, item2827, item2828, item2) 


all_SAF <- all_SAF %>% filter (saf_land ==1)


#join all_SAF and all_ags
#staged process as need to add columns depending on if SAF and/ or ags data exists for holdings; various inner and anti joins of both datasets are combined.


SAF_JAC_inner <- inner_join(all_SAF, all_ags, by = c('parish', 'holding'), all = TRUE,  suffix = c(".SAF", ".ags"))

SAF_JAC_SAF_anti <- anti_join(all_SAF, all_ags, by = c('parish', 'holding'), all = TRUE,  suffix = c(".SAF", ".ags"))

SAF_JAC_JAC_anti <- anti_join(all_ags, all_SAF, by = c('parish', 'holding'), all = TRUE,  suffix = c(".SAF", ".ags"))




both_SAF_JAC_inner_full <- SAF_JAC_inner %>%
  filter(survdata == "full") %>%
  mutate(land_data = as.factor("both"),
         other_data = as.factor("yes"),
         saf_data = as.factor("complete"),
         imptype = as.factor("none")
         )

#for 2023, delete partial- full only
both_SAF_JAC_inner_partial <- SAF_JAC_inner %>%
  filter(survdata == "part") %>%
  mutate(land_data = as.factor("saf"),
         other_data = as.factor("yes"),
         saf_data = as.factor("complete"),
         imptype = as.factor("none")
         )

SAF_anti <- SAF_JAC_SAF_anti %>%
  mutate(survdata = as.factor("none"),
         land_data = as.factor("saf"),
         other_data = as.factor("no"),
         imptype = as.factor("AGS"),
         survtype = as.factor("SAF_only"),
         saf_data = as.factor("complete")
         )

JAC_anti_full <- SAF_JAC_JAC_anti %>%
  filter(survdata == "full") %>%
  mutate(land_data = as.factor("ags"),
         other_data = as.factor("yes"),
         saf_data = as.factor("none"),
         imptype = as.factor("none")
         )

JAC_anti_partial <- SAF_JAC_JAC_anti %>%
  filter(survdata == "part") %>%
  mutate(land_data = as.factor("none"),
         other_data = as.factor("yes"),
         saf_data = as.factor("none"),
         imptype = as.factor("SAF")
                             )


inner_SAF_JAC <- bind_rows(both_SAF_JAC_inner_full, both_SAF_JAC_inner_partial)%>%
#keep only SAF data for inner-join data
select(!contains(".ags"))

#change names for binding with anti JAC and SAF.
names(inner_SAF_JAC) <- gsub(".SAF", "", names(inner_SAF_JAC) )
#names(inner_SAF_JAC)
anti_JAC <- bind_rows(JAC_anti_full, JAC_anti_partial)
full_JAC_SAF <-  bind_rows(inner_SAF_JAC, anti_JAC)
full_JAC_SAF <- bind_rows(full_JAC_SAF, SAF_anti)

#names(SAF_SEAS_ONLY)

#reorder columns
full_JAC_SAF <- full_JAC_SAF %>% select(parish, holding, survtype, imptype, survdata, land_data, saf_data,
                                        other_data, contains(all_items), contains(string_answers), mlc, brn, LFASS_area, everything())
full_JAC_SAF <- full_JAC_SAF %>% select(!c(item3166, submisType, maderight, item2726)) %>% 
arrange(by_group = TRUE)




#check if only SEASONAL SAF DATA exist and not in COMPLETE SAF + JAC data, add labels
full_JAC_SAF_SAF_SEAS_anti<- anti_join(SAF_SEAS_ONLY, full_JAC_SAF, by = c('parish', 'holding'), all = TRUE) %>%
  mutate(survdata = as.factor("none"),
         land_data = as.factor("none"),
         other_data = as.factor("no"),
         imptype = as.factor("full"),
         survtype = as.factor("None"),
         saf_data = as.factor("seasonal_only")
)


#Add 'seasonal_only' to saf_data column of entries in both complete SAF+JAC and SEASONAL ONLY
full_JAC_SAF_SAF_SEAS_inner<- inner_join(SAF_SEAS_ONLY, full_JAC_SAF, by = c('parish', 'holding'), all = TRUE, suffix = c(".SAF_SEAS", "JAC_SAF")) %>%
  mutate(saf_data = as.factor("seasonal_only")) %>%
  select(!contains("JAC_SAF")) %>%
  arrange(by_group = TRUE)

names(full_JAC_SAF_SAF_SEAS_inner) <- gsub(".SAF_SEAS", "", names(full_JAC_SAF_SAF_SEAS_inner) )




FJS_SEAS <- bind_rows(full_JAC_SAF_SAF_SEAS_inner, full_JAC_SAF_SAF_SEAS_anti)

FJS_anti <- anti_join(full_JAC_SAF, FJS_SEAS, by = c('parish', 'holding'), all = TRUE)
FJS <- bind_rows(FJS_anti,FJS_SEAS ) %>% arrange(by_group = TRUE)

#******QA **************WRITE CSV TO DO MANUAL CORRECTIONS IN SAS********************* QA****
write.csv(FJS, (paste0(output_path, "full_JAC_SAF_no_corr.csv")))


#read in manually corrected dataset
full_JAC_SAF_corr <- read.csv(paste0(output_path, "full_JAC_SAF_corr.csv")) %>% 
  group_by(parish, holding) %>% 
  arrange(by_group = TRUE)


check_glasshouse <- FJS %>% ungroup() %>% filter(land_data== "saf" & sum(item85, item86)>0)


#add in new items
#dealing with NAs - remove from sum to get value. Remove "na.rm = TRUE" to return NA (otherwise get zeros, imputation for genuinely missing values not zeros)

full_JAC_SAF <- full_JAC_SAF_corr %>% mutate(item87 = sum(item2713, item2707, na.rm = TRUE
                                                                ),
                                         item2036 = sum(item2714, item2708, na.rm = TRUE
                                                        ),
                                                             
                                         item2037 = sum(item2715, item2709, na.rm = TRUE
                                                        ),
                                                              
                                         
                                         item1711 = sum(item2716, item2710, na.rm = TRUE
                                                        ),
                                         item1943 =sum(item2717, item2711, na.rm = TRUE
                                                       ),
                                         
                                         item2556 = sum(item2858, item2863, na.rm = TRUE
                                                        ),
                                     
                                         item2557 = sum(item2859, item2864, na.rm = TRUE
                                                        ),
                                         
                                         item2836 = sum(item2860, item2865, na.rm = TRUE
                                                        ),
                                         
                                         item6000 = sum(item2861, item2866, na.rm = TRUE
                                         ),
                                         item6001 = sum(item2862, item2867, na.rm = TRUE
                                                        ), 
                                        
                                        item68=sum(item52,item53,item55,item56,item2323,item59,item60,item61,item63,item64,item65,item66, na.rm = TRUE),
                                        item35=item68,
                                        item38=sum(item41,item85,item86,item2324,item1709,item80,item81,item1710,item82,item83, na.rm = TRUE),
                                        item76=sum(item70,item71,item72,item2832,item75, na.rm = TRUE),
                                        item37=item76,
                                        item40=sum(item14,item15,item16,item18,item17,item20,item22,item19,item23,item21,item24,
                                                   item2320,item27,item28,item2034,item29,item30,item31,item2059,item32,item34,item35,
                                                   item36,item76,item38,item2469,item2470, item3156, na.rm = TRUE),
                                        
                                        item46=sum(item40, item2321, item2322, na.rm = TRUE),
                                        item84=sum(item2324, item1709, item80, item81,item1710, item82, item83, na.rm = TRUE),
                                        item87= sum(item2713, item2707, na.rm = TRUE) 
                                        
)


full_JAC_SAF <- full_JAC_SAF %>% mutate(item85 = case_when(survtype == "SAF" & land_data == "saf"  ~ sum(item2713, item2858, item2859, item2860, item2861, item2714, item2715, item2716, item2717, item2862, na.rm = TRUE),
                                                           TRUE~ as.numeric(item85)),
                                             
                                                        
                                        item86 = case_when(survtype == "SAF" & land_data == "saf"  ~sum(item2707, item2863, item2864, item2865, item2866, item2708, item2709, item2710, item2711, item2867, na.rm = TRUE),
                                                                                                        TRUE ~as.numeric(item86))
                                                           
                                                        

)

full_JAC_SAF <- full_JAC_SAF %>% mutate(item2 = case_when(sum(item2827,item2828) > 0  ~ sum(item2827,item2828),
                                                          TRUE~ as.numeric(item2)),
                                        item2879 =  case_when(sum(item2827,item2828) > 0  ~ 0,
                                                              TRUE ~ as.numeric(item2879))
)

full_JAC_SAF <- full_JAC_SAF %>% mutate(item2 = case_when(item2879 > 0  ~ as.numeric(item2879),
                                                          TRUE~ as.numeric(item2)),
                                        item2827 = case_when(item2879 > 0  ~ NA_real_,
                                                              TRUE ~ as.numeric(item2827)),
                                        item2828 = case_when(item2879 > 0 ~ NA_real_,
                                                                                  TRUE ~ as.numeric(item2828))
)

full_JAC_SAF <- full_JAC_SAF %>% arrange(parish, holding, by_group = TRUE)

#clear unused dataframes
rm(anti_JAC, both_SAF_JAC_inner_full, both_SAF_JAC_inner_partial, full_JAC_SAF_SAF_SEAS_anti,
   full_JAC_SAF_SAF_SEAS_inner, inner_SAF_JAC, JAC_anti_full, JAC_anti_partial,  SAF_anti, SAF_JAC_inner,
   SAF_JAC_JAC_anti, SAF_JAC_SAF_anti)

####checks with SAS outputs##########

SAS_combined <- read.csv(paste0(output_path, "COMBINED_DATA.csv"))

#order columns according to SAS_combined
full_JAC_SAF <- full_JAC_SAF[names(SAS_combined)]
FJS <- full_JAC_SAF %>% select(-c(i, item185, item186))
SAS <- SAS_combined %>% select(-c(i, item185, item186))

                           
#compare column classes
FJS_SAS_compare <- as.data.frame(compare_df_cols(FJS, SAS))



levels(FJS$survtype)[levels(FJS$survtype) == "SAF_only"] <- "SAF only"
levels(FJS$saf_data)[levels(FJS$saf_data) == "seasonal"] <- "seasonal only"




comb_fjs <-setdiff(FJS, SAS)
comb_fjs2 <- setdiff(SAS, FJS)


#files to explore differences in excel

write.csv(comb_fjs, (paste0(output_path, "june_diff.csv")))
write.csv(comb_fjs2, (paste0(output_path, "SAS_diff.csv")))


