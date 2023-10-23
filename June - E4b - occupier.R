# Before import -----------------------------------------------------------


# Clear environment prior

rm(list = ls())

#yr = this year 20xx
yr <- 2023
#yr1 <- yr - 2
#yr2 <-  yr - 3
#yr3 <-  yr - 4
# Load packages
library(plyr)
library(tidyverse)
library(RtoSQLServer)
library(writexl)






#<-Datashare<-file path for import and export
Code_directory<-("//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Main/June23")
sas_agstemp_path <-   "//s0177a/sasdata1/ags/census/agstemp/"
sas_agscens_path <-   "//s0177a/sasdata1/ags/census/agscens/"

# ADM schema for export

server <-   "s0196a\\ADM"
database <-   "RuralAndEnvironmentalScienceFarmingStatistics"
schema <-   "juneagriculturalsurvey2023alpha"



#Import-------------------------------------------------------------

june_final <-  read_table_from_db(server=server, 
                                 database=database, 
                                 schema=schema, 
                                 table_name="JAC23_final_dataset")



#Occupier items ---------------------------------------------------------------------

#Occupier(s)
#Occupier 1 details - legal responsibility is assumed
occupier1_male <- "item2877"
occupier1_female <- "item2878"
#Year of birth – enter format ‘YYYY’
occupier1_year <- "item27785"
#Occupier 1 – proportion of time spent working on this location:
#full-time
occupier1_ft <- "item177"
#Part-time: half-time or more
occupier1_pt_gthalf <- "item178"
#Part-time: less than half-time
occupier1_pt_lthalf <- "item179"
#no farm work on this location
occupier1_no_work <- "item2566"

#Occupier 2 details - Occupier 2 – can include formalised relationships (married couples and civil partnerships), 
#cohabiting couples and close family relationships (father, mother, daughter, son and sibling)
occupier2_male <- "item3056"
occupier2_female <- "item3057"
occupier2_legal_fin <- "item27805"
#Year of birth – enter format ‘YYYY’
occupier2_year <- "item27795"
#Occupier 2 – proportion of time spent working on this location:
#full-time
occupier2_ft <- "item182"
#Part-time: half-time or more
occupier2_pt_gthalf <- "item183"
#Part-time: less than half-time
occupier2_pt_lthalf <- "item184"
#no farm work on this location
occupier2_no_work <- "item2567"

# Age bands ---------------------------------------------------------------

# june_final <- june_final %>% mutate(age_band_occ1 = case_when(yr - get(occupier1_year)<25 & get(occupier1_year)>0 ~ "under_25",
#                                                        yr - get(occupier1_year)>=25 & yr - get(occupier1_year) <=34 ~ "25_to_34",
#                                                        yr - get(occupier1_year)>=35 & yr - get(occupier1_year) <=44 ~ "35_to_44",
#                                                        yr - get(occupier1_year)>=45 & yr - get(occupier1_year) <=54 ~ "45_to_54",
#                                                        yr - get(occupier1_year)>=55 & yr - get(occupier1_year) <=64 ~ "55_to_64",
#                                                        yr - get(occupier1_year)>=65 & get(occupier1_year)>0  ~ "over_64"),
#                                                        
#                                   age_band_occ2 = case_when(yr - get(occupier2_year)<25 & get(occupier2_year)>0 ~ "under_25",
#                                                             yr - get(occupier2_year)>=25 & yr - get(occupier2_year) <=34 ~ "25_to_34",
#                                                             yr - get(occupier2_year)>=35 & yr - get(occupier2_year) <=44 ~ "35_to_44",
#                                                             yr - get(occupier2_year)>=45 & yr - get(occupier2_year) <=54 ~ "45_to_54",
#                                                             yr - get(occupier2_year)>=55 & yr - get(occupier2_year) <=64 ~ "55_to_64",
#                                                             yr - get(occupier2_year)>=65 & get(occupier2_year)>0  ~ "over_64")
#                                   ) 


# Table categories --------------------------------------------------------

#Full time occupier
june_final <- june_final %>% mutate(ft_m_under_25_1 = case_when(get(occupier1_ft)>0 &
                                                                 get(occupier1_male) == 1 & 
                                                                 (yr - get(occupier1_year)<25 & get(occupier1_year)>0) ~ 1,
                                                               TRUE ~ 0),
                                    ft_m_under_25_2 = case_when(get(occupier2_ft)>0 &
                                                                 get(occupier2_male) == 1 & 
                                                                 (yr - get(occupier2_year)<25 & get(occupier2_year)>0) ~ 1,
                                                               TRUE ~ 0),
                                    
                                  ft_f_under_25_1 =case_when(get(occupier1_ft)>0 &
                                                             get(occupier1_female) == 1 & 
                                                             (yr - get(occupier1_year)<25 & get(occupier1_year)>0) ~ 1,
                                                             TRUE ~ 0),
                                                          
                                  ft_f_under_25_2 =case_when(get(occupier2_ft)>0 &
                                                               get(occupier2_female) == 1 & 
                                                               (yr - get(occupier2_year)<25 & get(occupier2_year)>0) ~ 1,
                                                             TRUE ~ 0),
                                  
                                  ft_m_25_to_34_1 = case_when(get(occupier1_ft)>0 &
                                                                get(occupier1_male) == 1 & 
                                                                (yr - get(occupier1_year)>=25 & yr - get(occupier1_year) <=34) ~ 1,
                                                              TRUE ~0),
                                  
                                  ft_m_25_to_34_2 = case_when(get(occupier2_ft)>0 &
                                                                get(occupier2_male) == 1 & 
                                                                (yr - get(occupier2_year)>=25 & yr - get(occupier2_year) <=34) ~1,
                                                              TRUE ~ 0),
                                  
                                  ft_f_25_to_34_1 = case_when(get(occupier1_ft)>0 &
                                                              get(occupier1_female) == 1 & 
                                                              (yr - get(occupier1_year)>=25 & yr - get(occupier1_year) <=34) ~ 1,
                                                              TRUE ~0),
                                                              
                                  ft_f_25_to_34_2= case_when(get(occupier2_ft)>0 &
                                                              get(occupier2_female) == 1 & 
                                                              (yr - get(occupier2_year)>=25 & yr - get(occupier2_year) <=34) ~1,
                                                            TRUE ~ 0),
                                  ft_m_35_to_44_1 = case_when(get(occupier1_ft)>0 &
                                                              get(occupier1_male) == 1 &
                                                              (yr - get(occupier1_year)>=35 & yr - get(occupier1_year) <=44) ~ 1,
                                                              TRUE ~ 0),
                                                            
                                  ft_m_35_to_44_2 = case_when(get(occupier2_ft)>0 &
                                                              get(occupier2_male) == 1 &
                                                              (yr - get(occupier2_year)>=35 & yr - get(occupier2_year) <=44) ~ 1,
                                                            TRUE ~ 0),
                                  ft_f_35_to_44_1 = case_when(get(occupier1_ft)>0 &
                                                              get(occupier1_female) == 1 &
                                                              (yr - get(occupier1_year)>=35 & yr - get(occupier1_year) <=44) ~ 1,
                                                              TRUE ~ 0),
                                  
                                  ft_f_35_to_44_2 = case_when(get(occupier2_ft)>0 &
                                                              get(occupier2_female) == 1 &
                                                              (yr - get(occupier2_year)>=35 & yr - get(occupier2_year) <=44) ~ 1,
                                                            TRUE ~ 0),
                                  
                                  ft_m_45_to_54_1 = case_when(get(occupier1_ft)>0 &
                                                              get(occupier1_male) == 1 &
                                                              (yr - get(occupier1_year)>=45 & yr - get(occupier1_year) <=54) ~ 1,
                                                              TRUE ~ 0),
                                                              
                                  ft_m_45_to_54_2 =case_when(get(occupier2_ft)>0 &
                                                              get(occupier2_male) == 1 &
                                                              (yr - get(occupier2_year)>=45 & yr - get(occupier2_year) <=54) ~ 1,
                                                            TRUE ~ 0),
                                  ft_f_45_to_54_1 = case_when(get(occupier1_ft)>0 &
                                                              get(occupier1_female) == 1 &
                                                              (yr - get(occupier1_year)>=45 & yr - get(occupier1_year) <=54) ~ 1,
                                                              TRUE ~ 0),
                                  
                                  ft_f_45_to_54_2 = case_when(get(occupier2_ft)>0 &
                                                              get(occupier2_female) == 1 &
                                                              (yr - get(occupier2_year)>=45 & yr - get(occupier2_year) <=54) ~ 1,
                                                            TRUE ~ 0),
                                  
                                  ft_m_55_to_64_1 = case_when(get(occupier1_ft)>0 &
                                                              get(occupier1_male) == 1 &
                                                              (yr - get(occupier1_year)>=55 & yr - get(occupier1_year) <=64) ~ 1,
                                                              TRUE ~ 0),
                                  ft_m_55_to_64_2 = case_when(get(occupier2_ft)>0 &
                                                              get(occupier2_male) == 1 &
                                                              (yr - get(occupier2_year)>=55 & yr - get(occupier2_year) <=64) ~ 1,
                                                            TRUE ~ 0),
                                  
                                  ft_f_55_to_64_1 = case_when(get(occupier1_ft)>0 &
                                                              get(occupier1_female) == 1 &
                                                              (yr - get(occupier1_year)>=55 & yr - get(occupier1_year) <=64) ~ 1,
                                                              TRUE ~ 0),
                                  
                                  ft_f_55_to_64_2= case_when(get(occupier2_ft)>0 &
                                                              get(occupier2_female) == 1 &
                                                              (yr - get(occupier2_year)>=55 & yr - get(occupier2_year) <=64) ~ 1,
                                                            TRUE ~ 0),
                                  ft_m_over_64_1 = case_when(get(occupier1_ft)>0 &
                                                             get(occupier1_male) ==1 &
                                                            (yr - get(occupier1_year)>=65 & get(occupier1_year)>0) ~ 1,
                                                            TRUE ~ 0),
                                  ft_m_over64_2 = case_when(get(occupier2_ft)>0 &
                                                             get(occupier2_male) ==1 &
                                                             (yr - get(occupier2_year)>=65 & get(occupier2_year)>0) ~ 1,
                                                           TRUE ~ 0),
                                  ft_f_over_64_1 = case_when(get(occupier1_ft)>0 &
                                                             get(occupier1_female) ==1 &
                                                             (yr - get(occupier1_year)>=65 & get(occupier1_year)>0) ~ 1,
                                                           TRUE ~ 0),
                                                           
                                  ft_f_over_64_2 = case_when(get(occupier2_ft)>0 &
                                                             get(occupier2_female) ==1 &
                                                             (yr - get(occupier2_year)>=65 & get(occupier2_year)>0) ~ 1,
                                                           TRUE ~ 0))
                                  
#Part time - half time or more
june_final <- june_final %>% mutate(pt_gthalf_m_under_25_1 = case_when(get(occupier1_pt_gthalf)>0 &
                                                                  get(occupier1_male) == 1 & 
                                                                  (yr - get(occupier1_year)<25 & get(occupier1_year)>0) ~ 1,
                                                                TRUE ~ 0),
                                    pt_gthalf_m_under_25_2 = case_when(get(occupier2_pt_gthalf)>0 &
                                                                  get(occupier2_male) == 1 & 
                                                                  (yr - get(occupier2_year)<25 & get(occupier2_year)>0) ~ 1,
                                                                TRUE ~ 0),
                                    
                                    pt_gthalf_f_under_25_1 =case_when(get(occupier1_pt_gthalf)>0 &
                                                                 get(occupier1_female) == 1 & 
                                                                 (yr - get(occupier1_year)<25 & get(occupier1_year)>0) ~ 1,
                                                               TRUE ~ 0),
                                    
                                    pt_gthalf_f_under_25_2 =case_when(get(occupier2_pt_gthalf)>0 &
                                                                 get(occupier2_female) == 1 & 
                                                                 (yr - get(occupier2_year)<25 & get(occupier2_year)>0) ~ 1,
                                                               TRUE ~ 0),
                                    
                                    pt_gthalf_m_25_to_34_1 = case_when(get(occupier1_pt_gthalf)>0 &
                                                                  get(occupier1_male) == 1 & 
                                                                  (yr - get(occupier1_year)>=25 & yr - get(occupier1_year) <=34) ~ 1,
                                                                TRUE ~0),
                                    
                                    pt_gthalf_m_25_to_34_2 = case_when(get(occupier2_pt_gthalf)>0 &
                                                                  get(occupier2_male) == 1 & 
                                                                  (yr - get(occupier2_year)>=25 & yr - get(occupier2_year) <=34) ~1,
                                                                TRUE ~ 0),
                                    
                                    pt_gthalf_f_25_to_34_1 = case_when(get(occupier1_pt_gthalf)>0 &
                                                                  get(occupier1_female) == 1 & 
                                                                  (yr - get(occupier1_year)>=25 & yr - get(occupier1_year) <=34) ~ 1,
                                                                TRUE ~0),
                                    
                                    pt_gthalf_f_25_to_34_2= case_when(get(occupier2_pt_gthalf)>0 &
                                                                 get(occupier2_female) == 1 & 
                                                                 (yr - get(occupier2_year)>=25 & yr - get(occupier2_year) <=34) ~1,
                                                               TRUE ~ 0),
                                    pt_gthalf_m_35_to_44_1 = case_when(get(occupier1_pt_gthalf)>0 &
                                                                  get(occupier1_male) == 1 &
                                                                  (yr - get(occupier1_year)>=35 & yr - get(occupier1_year) <=44) ~ 1,
                                                                TRUE ~ 0),
                                    
                                    pt_gthalf_m_35_to_44_2 = case_when(get(occupier2_pt_gthalf)>0 &
                                                                  get(occupier2_male) == 1 &
                                                                  (yr - get(occupier2_year)>=35 & yr - get(occupier2_year) <=44) ~ 1,
                                                                TRUE ~ 0),
                                    pt_gthalf_f_35_to_44_1 = case_when(get(occupier1_pt_gthalf)>0 &
                                                                  get(occupier1_female) == 1 &
                                                                  (yr - get(occupier1_year)>=35 & yr - get(occupier1_year) <=44) ~ 1,
                                                                TRUE ~ 0),
                                    
                                    pt_gthalf_f_35_to_44_2 = case_when(get(occupier2_pt_gthalf)>0 &
                                                                  get(occupier2_female) == 1 &
                                                                  (yr - get(occupier2_year)>=35 & yr - get(occupier2_year) <=44) ~ 1,
                                                                TRUE ~ 0),
                                    
                                    pt_gthalf_m_45_to_54_1 = case_when(get(occupier1_pt_gthalf)>0 &
                                                                  get(occupier1_male) == 1 &
                                                                  (yr - get(occupier1_year)>=45 & yr - get(occupier1_year) <=54) ~ 1,
                                                                TRUE ~ 0),
                                    
                                    pt_gthalf_m_45_to_54_2 =case_when(get(occupier2_pt_gthalf)>0 &
                                                                 get(occupier2_male) == 1 &
                                                                 (yr - get(occupier2_year)>=45 & yr - get(occupier2_year) <=54) ~ 1,
                                                               TRUE ~ 0),
                                    pt_gthalf_f_45_to_54_1 = case_when(get(occupier1_pt_gthalf)>0 &
                                                                  get(occupier1_female) == 1 &
                                                                  (yr - get(occupier1_year)>=45 & yr - get(occupier1_year) <=54) ~ 1,
                                                                TRUE ~ 0),
                                    
                                    pt_gthalf_f_45_to_54_2 = case_when(get(occupier2_pt_gthalf)>0 &
                                                                  get(occupier2_female) == 1 &
                                                                  (yr - get(occupier2_year)>=45 & yr - get(occupier2_year) <=54) ~ 1,
                                                                TRUE ~ 0),
                                    
                                    pt_gthalf_m_55_to_64_1 = case_when(get(occupier1_pt_gthalf)>0 &
                                                                  get(occupier1_male) == 1 &
                                                                  (yr - get(occupier1_year)>=55 & yr - get(occupier1_year) <=64) ~ 1,
                                                                TRUE ~ 0),
                                    pt_gthalf_m_55_to_64_2 = case_when(get(occupier2_pt_gthalf)>0 &
                                                                  get(occupier2_male) == 1 &
                                                                  (yr - get(occupier2_year)>=55 & yr - get(occupier2_year) <=64) ~ 1,
                                                                TRUE ~ 0),
                                    
                                    pt_gthalf_f_55_to_64_1 = case_when(get(occupier1_pt_gthalf)>0 &
                                                                  get(occupier1_female) == 1 &
                                                                  (yr - get(occupier1_year)>=55 & yr - get(occupier1_year) <=64) ~ 1,
                                                                TRUE ~ 0),
                                    
                                    pt_gthalf_f_55_to_64_2= case_when(get(occupier2_pt_gthalf)>0 &
                                                                 get(occupier2_female) == 1 &
                                                                 (yr - get(occupier2_year)>=55 & yr - get(occupier2_year) <=64) ~ 1,
                                                               TRUE ~ 0),
                                    pt_gthalf_m_over_64_1 = case_when(get(occupier1_pt_gthalf)>0 &
                                                                 get(occupier1_male) ==1 &
                                                                 (yr - get(occupier1_year)>=65 & get(occupier1_year)>0) ~ 1,
                                                               TRUE ~ 0),
                                    pt_gthalf_m_over64_2 = case_when(get(occupier2_pt_gthalf)>0 &
                                                                get(occupier2_male) ==1 &
                                                                (yr - get(occupier2_year)>=65 & get(occupier2_year)>0) ~ 1,
                                                              TRUE ~ 0),
                                    pt_gthalf_f_over_64_1 = case_when(get(occupier1_pt_gthalf)>0 &
                                                                 get(occupier1_female) ==1 &
                                                                 (yr - get(occupier1_year)>=65 & get(occupier1_year)>0) ~ 1,
                                                               TRUE ~ 0),
                                    
                                    pt_gthalf_f_over_64_2 = case_when(get(occupier2_pt_gthalf)>0 &
                                                                 get(occupier2_female) ==1 &
                                                                 (yr - get(occupier2_year)>=65 & get(occupier2_year)>0) ~ 1,
                                                               TRUE ~ 0))


#Part time - less than half time
june_final <- june_final %>% mutate(pt_lthalf_m_under_25_1 = case_when(get(occupier1_pt_lthalf)>0 &
                                                                  get(occupier1_male) == 1 & 
                                                                  (yr - get(occupier1_year)<25 & get(occupier1_year)>0) ~ 1,
                                                                TRUE ~ 0),
                                    pt_lthalf_m_under_25_2 = case_when(get(occupier2_pt_lthalf)>0 &
                                                                  get(occupier2_male) == 1 & 
                                                                  (yr - get(occupier2_year)<25 & get(occupier2_year)>0) ~ 1,
                                                                TRUE ~ 0),
                                    
                                    pt_lthalf_f_under_25_1 =case_when(get(occupier1_pt_lthalf)>0 &
                                                                 get(occupier1_female) == 1 & 
                                                                 (yr - get(occupier1_year)<25 & get(occupier1_year)>0) ~ 1,
                                                               TRUE ~ 0),
                                    
                                    pt_lthalf_f_under_25_2 =case_when(get(occupier2_pt_lthalf)>0 &
                                                                 get(occupier2_female) == 1 & 
                                                                 (yr - get(occupier2_year)<25 & get(occupier2_year)>0) ~ 1,
                                                               TRUE ~ 0),
                                    
                                    pt_lthalf_m_25_to_34_1 = case_when(get(occupier1_pt_lthalf)>0 &
                                                                  get(occupier1_male) == 1 & 
                                                                  (yr - get(occupier1_year)>=25 & yr - get(occupier1_year) <=34) ~ 1,
                                                                TRUE ~0),
                                    
                                    pt_lthalf_m_25_to_34_2 = case_when(get(occupier2_pt_lthalf)>0 &
                                                                  get(occupier2_male) == 1 & 
                                                                  (yr - get(occupier2_year)>=25 & yr - get(occupier2_year) <=34) ~1,
                                                                TRUE ~ 0),
                                    
                                    pt_lthalf_f_25_to_34_1 = case_when(get(occupier1_pt_lthalf)>0 &
                                                                  get(occupier1_female) == 1 & 
                                                                  (yr - get(occupier1_year)>=25 & yr - get(occupier1_year) <=34) ~ 1,
                                                                TRUE ~0),
                                    
                                    pt_lthalf_f_25_to_34_2= case_when(get(occupier2_pt_lthalf)>0 &
                                                                 get(occupier2_female) == 1 & 
                                                                 (yr - get(occupier2_year)>=25 & yr - get(occupier2_year) <=34) ~1,
                                                               TRUE ~ 0),
                                    pt_lthalf_m_35_to_44_1 = case_when(get(occupier1_pt_lthalf)>0 &
                                                                  get(occupier1_male) == 1 &
                                                                  (yr - get(occupier1_year)>=35 & yr - get(occupier1_year) <=44) ~ 1,
                                                                TRUE ~ 0),
                                    
                                    pt_lthalf_m_35_to_44_2 = case_when(get(occupier2_pt_lthalf)>0 &
                                                                  get(occupier2_male) == 1 &
                                                                  (yr - get(occupier2_year)>=35 & yr - get(occupier2_year) <=44) ~ 1,
                                                                TRUE ~ 0),
                                    pt_lthalf_f_35_to_44_1 = case_when(get(occupier1_pt_lthalf)>0 &
                                                                  get(occupier1_female) == 1 &
                                                                  (yr - get(occupier1_year)>=35 & yr - get(occupier1_year) <=44) ~ 1,
                                                                TRUE ~ 0),
                                    
                                    pt_lthalf_f_35_to_44_2 = case_when(get(occupier2_pt_lthalf)>0 &
                                                                  get(occupier2_female) == 1 &
                                                                  (yr - get(occupier2_year)>=35 & yr - get(occupier2_year) <=44) ~ 1,
                                                                TRUE ~ 0),
                                    
                                    pt_lthalf_m_45_to_54_1 = case_when(get(occupier1_pt_lthalf)>0 &
                                                                  get(occupier1_male) == 1 &
                                                                  (yr - get(occupier1_year)>=45 & yr - get(occupier1_year) <=54) ~ 1,
                                                                TRUE ~ 0),
                                    
                                    pt_lthalf_m_45_to_54_2 =case_when(get(occupier2_pt_lthalf)>0 &
                                                                 get(occupier2_male) == 1 &
                                                                 (yr - get(occupier2_year)>=45 & yr - get(occupier2_year) <=54) ~ 1,
                                                               TRUE ~ 0),
                                    pt_lthalf_f_45_to_54_1 = case_when(get(occupier1_pt_lthalf)>0 &
                                                                  get(occupier1_female) == 1 &
                                                                  (yr - get(occupier1_year)>=45 & yr - get(occupier1_year) <=54) ~ 1,
                                                                TRUE ~ 0),
                                    
                                    pt_lthalf_f_45_to_54_2 = case_when(get(occupier2_pt_lthalf)>0 &
                                                                  get(occupier2_female) == 1 &
                                                                  (yr - get(occupier2_year)>=45 & yr - get(occupier2_year) <=54) ~ 1,
                                                                TRUE ~ 0),
                                    
                                    pt_lthalf_m_55_to_64_1 = case_when(get(occupier1_pt_lthalf)>0 &
                                                                  get(occupier1_male) == 1 &
                                                                  (yr - get(occupier1_year)>=55 & yr - get(occupier1_year) <=64) ~ 1,
                                                                TRUE ~ 0),
                                    pt_lthalf_m_55_to_64_2 = case_when(get(occupier2_pt_lthalf)>0 &
                                                                  get(occupier2_male) == 1 &
                                                                  (yr - get(occupier2_year)>=55 & yr - get(occupier2_year) <=64) ~ 1,
                                                                TRUE ~ 0),
                                    
                                    pt_lthalf_f_55_to_64_1 = case_when(get(occupier1_pt_lthalf)>0 &
                                                                  get(occupier1_female) == 1 &
                                                                  (yr - get(occupier1_year)>=55 & yr - get(occupier1_year) <=64) ~ 1,
                                                                TRUE ~ 0),
                                    
                                    pt_lthalf_f_55_to_64_2= case_when(get(occupier2_pt_lthalf)>0 &
                                                                 get(occupier2_female) == 1 &
                                                                 (yr - get(occupier2_year)>=55 & yr - get(occupier2_year) <=64) ~ 1,
                                                               TRUE ~ 0),
                                    pt_lthalf_m_over_64_1 = case_when(get(occupier1_pt_lthalf)>0 &
                                                                 get(occupier1_male) ==1 &
                                                                 (yr - get(occupier1_year)>=65 & get(occupier1_year)>0) ~ 1,
                                                               TRUE ~ 0),
                                    pt_lthalf_m_over64_2 = case_when(get(occupier2_pt_lthalf)>0 &
                                                                get(occupier2_male) ==1 &
                                                                (yr - get(occupier2_year)>=65 & get(occupier2_year)>0) ~ 1,
                                                              TRUE ~ 0),
                                    pt_lthalf_f_over_64_1 = case_when(get(occupier1_pt_lthalf)>0 &
                                                                 get(occupier1_female) ==1 &
                                                                 (yr - get(occupier1_year)>=65 & get(occupier1_year)>0) ~ 1,
                                                               TRUE ~ 0),
                                    
                                    pt_lthalf_f_over_64_2 = case_when(get(occupier2_pt_lthalf)>0 &
                                                                 get(occupier2_female) ==1 &
                                                                 (yr - get(occupier2_year)>=65 & get(occupier2_year)>0) ~ 1,
                                                               TRUE ~ 0))



# Checks ------------------------------------------------------------------



check <- june_final %>% select(any_of(c(occupier1_year,
                                        occupier2_year,
                                        occupier1_female,
                                        occupier1_male,
                                        occupier2_female,
                                        occupier2_male,
                                        occupier1_pt_lthalf,
                                        occupier2_pt_lthalf
)), age_band_occ1, age_band_occ2,
starts_with("ft"))


check <- june_final %>% select(any_of(c(occupier1_year,
                                        occupier2_year,
                                        occupier1_female,
                                        occupier1_male,
                                        occupier2_female,
                                        occupier2_male,
                                        occupier1_pt_gthalf,
                                        occupier2_pt_gthalf, 
                                        occupier1_pt_lthalf,
                                        occupier2_pt_lthalf
)), age_band_occ1, age_band_occ2,
starts_with("pt"))


# Totals ------------------------------------------------------------------
occupier <- june_final %>% select(any_of(c(occupier1_year,
                                                  occupier2_year,
                                                  occupier1_female,
                                                  occupier1_male,
                                                  occupier2_female,
                                                  occupier2_male,
                                                  occupier1_ft, 
                                                  occupier2_ft,
                                                  occupier1_pt_gthalf,
                                                  occupier2_pt_gthalf, 
                                                  occupier1_pt_lthalf,
                                                  occupier2_pt_lthalf)),
                                         age_band_occ1, 
                                         age_band_occ2,
                                         starts_with(c("ft", "pt_")))

 occupier_total <- data.frame(ft_m_under_25= sum(occupier$ft_m_under_25_1, occupier$ft_m_under_25_2, na.rm=TRUE),
                              ft_f_under_25 = sum(occupier$ft_f_under_25_1,occupier$ft_f_under_25_2, na.rm =TRUE),
                              ft_m_25_to_34 = sum(occupier$ft_m_25_to_34_1, occupier$ft_m_25_to_34_2, na.rm=TRUE),
                              ft_f_25_to_34 = sum(occupier$ft_f_25_to_34_1, occupier$ft_f_25_to_34_2, na.rm=TRUE),
                              ft_m_35_to_44 = sum(occupier$ft_m_35_to_44_1, occupier$ft_m_35_to_44_2,  na.rm=TRUE),
                              ft_f_35_to_44 = sum(occupier$ft_f_35_to_44_1, occupier$ft_f_35_to_44_2, na.rm=TRUE),
                              ft_m_45_to_54 = sum(occupier$ft_m_45_to_54_1, occupier$ft_m_45_to_54_2, na.rm=TRUE),
                              ft_f_45_to_54 = sum(occupier$ft_f_45_to_54_1, occupier$ft_f_45_to_54_2, na.rm=TRUE),
                              ft_m_55_to_64 = sum(occupier$ft_m_55_to_64_1, occupier$ft_m_55_to_64_2,  na.rm=TRUE),
                              ft_f_55_to_64 = sum(occupier$ft_f_55_to_64_1, occupier$ft_f_55_to_64_2,  na.rm=TRUE),
                              ft_m_over_64 = sum(occupier$ft_m_over_64_1,   occupier$ft_m_over_64_2, na.rm=TRUE),
                              ft_f_over_64 = sum(occupier$ft_f_over_64_1,   occupier$ft_f_over_64_2, na.rm=TRUE),
                              pt_gthalf_m_under_25= sum(occupier$pt_gthalf_m_under_25_1,  occupier$pt_gthalf_m_under_25_2, na.rm=TRUE),
                              pt_gthalf_f_under_25 = sum(occupier$pt_gthalf_f_under_25_1, occupier$pt_gthalf_f_under_25_2,  na.rm =TRUE),
                              pt_gthalf_m_25_to_34 = sum(occupier$pt_gthalf_m_25_to_34_1, occupier$pt_gthalf_m_25_to_34_2, na.rm=TRUE),
                              pt_gthalf_f_25_to_34 = sum(occupier$pt_gthalf_f_25_to_34_1, occupier$pt_gthalf_f_25_to_34_2, na.rm=TRUE),
                              pt_gthalf_m_35_to_44 = sum(occupier$pt_gthalf_m_35_to_44_1, occupier$pt_gthalf_m_35_to_44_2, na.rm=TRUE),
                              pt_gthalf_f_35_to_44 = sum(occupier$pt_gthalf_f_35_to_44_1, occupier$pt_gthalf_f_35_to_44_2, na.rm=TRUE),
                              pt_gthalf_m_45_to_54 = sum(occupier$pt_gthalf_m_45_to_54_1, occupier$pt_gthalf_m_45_to_54_2, na.rm=TRUE),
                              pt_gthalf_f_45_to_54 = sum(occupier$pt_gthalf_f_45_to_54_1, occupier$pt_gthalf_f_45_to_54_2, na.rm=TRUE),
                              pt_gthalf_m_55_to_64 = sum(occupier$pt_gthalf_m_55_to_64_1, occupier$pt_gthalf_m_55_to_64_2, na.rm=TRUE),
                              pt_gthalf_f_55_to_64 = sum(occupier$pt_gthalf_f_55_to_64_1, occupier$pt_gthalf_f_55_to_64_2,  na.rm=TRUE),
                              pt_gthalf_m_over_64 = sum(occupier$pt_gthalf_m_over_64_1, occupier$pt_gthalf_m_over_64_2, na.rm=TRUE),
                              pt_gthalf_f_over_64 = sum(occupier$pt_gthalf_f_over_64_1, occupier$pt_gthalf_f_over_64_2, na.rm=TRUE),
                              pt_lthalf_m_under_25= sum(occupier$pt_lthalf_m_under_25_1, occupier$pt_lthalf_m_under_25_2,  na.rm=TRUE),
                              pt_lthalf_f_under_25 = sum(occupier$pt_lthalf_f_under_25_1, occupier$pt_lthalf_f_under_25_2, na.rm =TRUE),
                              pt_lthalf_m_25_to_34 = sum(occupier$pt_lthalf_m_25_to_34_1, occupier$pt_lthalf_m_25_to_34_2, na.rm=TRUE),
                              pt_lthalf_f_25_to_34 = sum(occupier$pt_lthalf_f_25_to_34_1, occupier$pt_lthalf_f_25_to_34_2, na.rm=TRUE),
                              pt_lthalf_m_35_to_44 = sum(occupier$pt_lthalf_m_35_to_44_1, occupier$pt_lthalf_m_35_to_44_2, na.rm=TRUE),
                              pt_lthalf_f_35_to_44 = sum(occupier$pt_lthalf_f_35_to_44_1, occupier$pt_lthalf_f_35_to_44_2, na.rm=TRUE),
                              pt_lthalf_m_45_to_54 = sum(occupier$pt_lthalf_m_45_to_54_1, occupier$pt_lthalf_m_45_to_54_2, na.rm=TRUE),
                              pt_lthalf_f_45_to_54 = sum(occupier$pt_lthalf_f_45_to_54_1, occupier$pt_lthalf_f_45_to_54_2, na.rm=TRUE),
                              pt_lthalf_m_55_to_64 = sum(occupier$pt_lthalf_m_55_to_64_1, occupier$pt_lthalf_m_55_to_64_2, na.rm=TRUE),
                              pt_lthalf_f_55_to_64 = sum(occupier$pt_lthalf_f_55_to_64_1, occupier$pt_lthalf_f_55_to_64_2, na.rm=TRUE),
                              pt_lthalf_m_over_64 = sum(occupier$pt_lthalf_m_over_64_1, occupier$pt_lthalf_m_over_64_2, na.rm=TRUE),
                              pt_lthalf_f_over_64 = sum(occupier$pt_lthalf_f_over_64_1, occupier$pt_lthalf_f_over_64_2, na.rm=TRUE)
                              
                              )                                 

 
 occupier_pivot <- occupier_total %>% pivot_longer(everything(), names_to = "categories", values_to = "count")
write.csv(occupier_pivot, file= paste0(Code_directory, "/occupier23agesplit.csv"))

