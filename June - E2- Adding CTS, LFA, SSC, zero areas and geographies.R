# Before import -----------------------------------------------------------


# Clear environment prior

rm(list = ls())


# Load packages

library(tidyverse)
library(RtoSQLServer)
library(readxl)


source("June - E1 - ScotEID.R")

yr <- 2023
yr1 <- yr - 1
yr2 <-  yr - 2
yr3 <-  yr - 3
yr_list <- c(yr, yr2, yr3)


#<-Datashare<-file path for import and export
Code_directory<-("//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Codeconversion_2023/2023")
sas_agstemp_path <-   "//s0177a/sasdata1/ags/census/agstemp/"
sas_agscens_path <-   "//s0177a/sasdata1/ags/census/agscens/"

# ADM schema for export

server <-   "s0196a\\ADM"
database <-   "RuralAndEnvironmentalScienceFarmingStatistics"
schema <-   "juneagriculturalsurvey2023alpha"


# Load functions

# Functions ---------------------------------------------------------------
remove_id <- function(x) { x <-  x %>% select(-contains("ID", ignore.case = FALSE))  }

recode_response <- function(x) {
  ifelse(x != 0 | !is.na(x), 1, 0)
}
recode_number_response <- function(x) {
  x %>%
    mutate(number_responses = rowSums(.))
}

#June Item categories
# Item categories ---------------------------------------------------------

all_holding_areas <- c("item11", "item12", "item20026")
all_seasonal <- c("item1", "item2827", "item2828", "item2")
all_seasonal_saf <- "item1"
all_glasshouse_ags <- c("item2713", "item2707", "item2866", "item2714", "item2708", "item2715", 
                        "item2709", "item2716", "item2710", "item2717", "item2711", "item2862", "item2867")
all_glasshouse_saf <- c("item2858", "item2859", "item2860", "item2861", "item2863", "item2864", "item2865")
all_glasshouse <- c(all_glasshouse_ags, all_glasshouse_saf, "item85", "item86")
all_cattle <- c("cts301", "cts302", "cts303", "cts304", "cts305", "cts306", "cts307", "cts308", "cts309", "cts310", "cts311", "cts312")
all_sheep <- c("item139", "item140", "item141", "item144", "item145")
all_crops <- c("item14", "item15", "item16", "item18", "item17", "item20", "item3156", "item22", "item19", 
               "item23", "item21", "item24", "item2320", "item27", "item28", "item2034", "item29", "item30",
               "item31", "item2059", "item32", "item34", "item36", "item2469", "item2470")
all_grass <- c("item2321", "item2322")
all_other_land <- c("item47", "item48", "item49")
all_veg  <- c("item52", "item53", "item55", "item56", "item2323", "item59", "item60", "item61", "item63", "item64", "item65", "item66")
all_fruit <- c("item70", "item71", "item72", "item2832", "item75")
all_nursery <- c("item2324", "item1709", "item80", "item81", "item1710", "item82", "item83")
all_pig <-c("item146", "item147", "item148", "item149", "item50", "item151", "item27760","item27765", "item27770", "item157")
all_poultry <- c("item158", "item159", "item160", "item161", "item162", "item163", "item164", "item167", "item170", "item1708", "item2038", "item2039")
all_other_livestock <- c("item94", "item95", "item96", "item1712", "item1713", "item98", "item2472", "item2473", "item2474", "item2826", "item2868", "item171")
occupier_12 <- c("item2877","item2878", "item27785","item177","item178", "item179", "item2566","item3056","item3057", "item27805", "item27795", "item182","item183","item184", "item2567")
workforce <- c("item1714", "item1715", "item1716", "item1717", "item192", "item193", "item1718", "item194", "item195", "item1719", "item196", "item197", "item198", "item199", "item200")
other_labour <- c("item2712", "item2066", "item2511")
ags_leftover <- c("item48", "item2980")
string_answers <- c("item185", "item186")


#all JAC items
all_items <- c(all_holding_areas, all_seasonal, all_seasonal_saf, 
               "item2", all_crops, "item35", "item37", "item38", "item40", "item41", 
               all_grass, "item46", all_other_land, "item50", all_veg, "item68",
               all_fruit, "item76", all_nursery, "item84", all_glasshouse, "item85", "item86", "item2836",
               all_cattle, "cts312", all_sheep, "item145", all_pig, "item157", all_poultry, "item170",
               all_other_livestock, "item171", occupier_12, workforce, "item200", other_labour, "item2980") 

#Import-------------------------------------------------------------
# Cattle data from  


#Post imputation June data
June_data <-  read_table_from_db(server=server, 
                                database=database, 
                                schema=schema, 
                                table_name="post_imputation")

#address file for LFAs
addressfileorig <-  read_table_from_db(server=server, 
                                    database=database, 
                                    schema=schema, 
                                    table_name="address_occid_01jun_2023")
# 
# #unitary authorities for geog
unit_auth <-  read_table_from_db(server=server,
                                 database=database,
                                 schema=schema, 
                                 table_name="unitary_authorities")
# #Nuts geographies
nuts_2018 <-  read_table_from_db(server=server,
                                 database=database,
                                 schema=schema,
                                 table_name="NUTS_2018_ITL2")

#formats
agscens_format <-read_table_from_db(server = server,
                                    database = database,
                                    schema = schema,
                                    table_name = "agscens_SAS_format")


#previous year's data (2021)

prev_yr <-  read_table_from_db(
  server = server,
  database = database,
  schema = schema,
  table_name = "jac_2021_update"
)

# Add CTS data to June dataset --------------------------------------------

cts_var <-  paste0("cts", 301:312)

#add landless flag before join
june_data <-   June_data %>% mutate(landless = case_when(item50>0 ~ 0,
                                                       TRUE ~ 1),
                                  completedata = case_when(item50>0| 
                                                             item145>0|
                                                             item157>0|
                                                             item170>0 ~ 1,
                                                           TRUE ~ 0)
                                  )

#Holdings with only CTS data - add flags 
CTS_only <-   anti_join(cts, june_data, by = c("parish", "holding")) %>% 
  mutate(survtype = "CTS only",
         survdata = "CTS only",
         land_data = "landless",
         other_data = "CTS only",
         imptype = "n/a",
         ags_madeup = 99,
         saf_madeup = 99,
         madeup = 99
         )

#Holdings without CTS data- add i flag
june_only <-  anti_join(june_data, cts, by = c("parish", "holding")) %>% 
  mutate(i = 1 + length(cts_var))

#inner join June and CTS
june_cts_inner <- inner_join(june_data, cts, by = c("parish", "holding"))

#append  
june_cts <-  bind_rows(june_only, june_cts_inner, CTS_only)




# Add common grazing to dataset -------------------------------------------

addressfile <-  addressfileorig %>% 
  select(Parish, Holding, Holding_Classification_Id, TotArea)

addressfile <- addressfile %>%  dplyr::rename(parish = "Parish",
         holding = "Holding")
   
#mutate common grazing area = total area  for holding classification ID =3
common_graze <-  addressfile %>% mutate(cga =case_when(Holding_Classification_Id==3 ~ as.numeric(TotArea)) ) %>% 
  filter(Holding_Classification_Id == 3) %>% select(-Holding_Classification_Id, -TotArea)

#add to june dataset

#check if any holdings only appear in common graze data NOT june, add flags
cg_only <-   anti_join(common_graze, june_cts, by = c("parish", "holding")) %>% 
  mutate(completedata = 0,
         survtype = "C/Grazings",
         survdata = "C/Grazings",
         land_data = "landless",
         other_data = "C/Grazings",
         imptype = "n/a",
         ags_madeup = 99,
         saf_madeup = 99,
         madeup= 99
         )

#append
june_cts_cg<-  bind_rows(june_cts, cg_only) 


# Add land area, LFA variables and flags ----------------------------------

#LFA areas are taken from the address file, therefore can calculate these for most landless cattle keepers
#where no address file match is found, then assume LFA is type 1 (SDA)


addressfile <-   addressfileorig %>% 
  select(c(Parish,
           Holding,
           TotDaArea, 
           TotSdaArea,
           TotNonLfaArea,
           Name,
           Holding_Classification_Id)) %>% 
  dplyr::rename(parish = "Parish",
         holding = "Holding")

#inner join and mutate lfa vars
june_lfa_inner <-  inner_join(june_cts_cg, addressfile, by = c("parish", "holding")) %>% 
  mutate(TotArea = TotDaArea + TotSdaArea + TotNonLfaArea,
         item125 = TotSdaArea,
         item126 = TotDaArea,
         item127 = TotNonLfaArea,
         lfatemp = case_when(item50 ==0 | TotArea == 0 ~ 1,
                             item127 == TotArea ~ 3,
                             item126 == TotArea ~ 2,
                             item125 == TotArea ~ 1,
                             ((item125 + item126)>=0.5*TotArea) &
                               (item125/(item125 + item126)>0.5) ~ 1,
                             ((item125 + item126)>=0.5*TotArea) &
                               (item126/(item125 + item126)>0.5) ~ 2,
                             item125 + item126 <0.5 *(TotArea) ~ 3
                             )
         )

june_only <-  anti_join(june_cts_cg, addressfile, by = c("parish", "holding")) %>% 
  mutate(lfatemp = 1)

#append
june_lfa <-   bind_rows(june_lfa_inner, june_only)

#check
#lfa_only  anti_join(addressfile, june_cts_cg, by = c("parish", "holding"))


#add more vars to dataset- 
june_lfa <-   june_lfa %>% mutate(holdclas = as.integer(Holding_Classification_Id),
                                holdtype= Name,
                                #delete below no item7
                                # tenure = case_when(completedata ==1 & 
                                #                      item11 == item12 ~1,
                                #                    completedata == 1 &
                                #                      item7 == item12 ~ 2,
                                #                    completedata == 1 &
                                #                      (item11 !=item12) & (item7 !=item12) ~ 3),
                                saf_completed = case_when(completedata == 1 & 
                                                            land_data %in% c("saf", "both") ~ 1,
                                                          completedata == 1 &
                                                            land_data %in% c("ags", "none", "landless") ~ 0
                                                   )) %>% 
  select(-Holding_Classification_Id, Name)




# Fix Sheep Stock Club Areas ----------------------------------------------
#SSC areas should be zero
ssc_cols <-c(all_holding_areas, 
           all_seasonal,
           all_crops,
           "item50",
           "item35",
           "item37",
           "item38",
           "item40",
           "item41",
           "item46",
           all_grass,
           all_other_land
           )

june_lfa<- june_lfa %>% mutate(across(any_of(ssc_cols), ~ifelse(holdclas==4 & !is.na(holdclas), 0, .)))
june_lfa <- june_lfa %>% mutate(landless = case_when(holdclas==4 & !is.na(holdclas) ~ 0,
                                                     TRUE ~as.numeric(landless)),
                                completedata = case_when(holdclas==4 & !is.na(holdclas) ~ 1,
                                                         TRUE ~ as.numeric(completedata))
)
  
# Add Unitary Authority code to June Dataset --------------------------------
#remove ID cols
june_lfa <- remove_id(june_lfa)
unit_auth <- remove_id(unit_auth)

#join unit_auth with june data

june_geog <- full_join(june_lfa, unit_auth, by = "parish")

# 
# # Add NUTS code and ITL2 to June Dataset--------------------------------------------------
nuts_2018 <- nuts_2018 %>%
  select(parish,
         Parish_Name,
         nuts2_2018,
         nuts3_2018,
         nuts4_2018,
         ITL2_2021_code)

nuts_2018 <- nuts_2018%>%
  dplyr::rename(NUTS2= "nuts2_2018",
         NUTS3= "nuts3_2018",
         NUTS4= "nuts4_2018")

june_geog <- full_join(june_geog, nuts_2018, by = "parish")

# agscens format geographies ----------------------------------------

#extract geographies from agscens format table
geog <- agscens_format %>%
  filter(FMTNAME %in% c("PARISH",
                        "DISTRICT",
                        "NUT2SN",
                        "NUT2S",
                        "NUT3S",
                        "NUT4S",
                        "AGRICREG",
                        "COUNTY",
                        "REGALT"
  )
) %>% 
  select(FMTNAME, START, LABEL)

#lowercase column names
names(geog) <- tolower(names(geog))

#parish
parish <- geog %>% ungroup %>%  
  filter(fmtname=="PARISH") %>%
  dplyr::rename(parish = "start") %>% 
  select(parish, label)

#district
district <- geog %>% ungroup %>%  
  filter(fmtname=="DISTRICT") %>%
  dplyr::rename(district = "start",
         district_label= "label") %>% 
  select(district, district_label)

#make numeric before join
district$district <-as.numeric(district$district) 

#region
region <- geog %>% ungroup %>%  
  filter(fmtname=="REGALT") %>%
  dplyr::rename(region = "start",
         region_label= "label") %>% 
  select(region, region_label)

#make numeric before join
region$region <-as.numeric(region$region) 

#agricultural region
agric_reg <- geog %>% ungroup %>%  
  filter(fmtname=="AGRICREG") %>%
  dplyr::rename(agricreg = "start",
         agric_reg_label= "label") %>% 
  select(agricreg, agric_reg_label)

#make numeric before join
agric_reg$agricreg <-as.numeric(agric_reg$agricreg) 

#county
county <- geog %>% ungroup %>%  
  filter(fmtname=="COUNTY") %>%
  dplyr::rename(county = "start",
         county_label= "label") %>% 
  select(county, county_label)

#make numeric before join
county$county <-as.numeric(county$county) 

#NUTS
nuts2 <- geog %>% ungroup %>%  
  filter(fmtname=="NUT2S") %>%
  dplyr::rename(NUTS2 = "start",
         NUTS2_label = "label") %>% 
  select(NUTS2, NUTS2_label) 

#make numeric before join
nuts2$NUTS2<- as.numeric(nuts2$NUTS2)

nuts3 <- geog %>% ungroup %>%  
  filter(fmtname=="NUT3S") %>%
  dplyr::rename(NUTS3 = "start",
         NUTS3_label = "label") %>% 
  select(NUTS3, NUTS3_label)

#make numeric before join
nuts3$NUTS3<- as.numeric(nuts3$NUTS3)

nuts4 <- geog %>% ungroup %>%  
  filter(fmtname=="NUT4S") %>%
  dplyr::rename(NUTS4 = "start",
         NUTS4_label = "label") %>% 
  select(NUTS4, NUTS4_label)

#make numeric before join
nuts4$NUTS4<- as.numeric(nuts4$NUTS4)




# #Add NUTS labels to june dataset -----------------------------------------------
june_geog <- full_join(june_geog, nuts2, by = "NUTS2") 
june_geog <- full_join(june_geog, nuts3, by = "NUTS3")
june_geog <- full_join(june_geog, nuts4, by = "NUTS4")  


# Add agric region codes to June dataset ----------------------------------

june_geog <- june_geog %>% 
  mutate(agricreg= case_when(unitary_authority == 360 ~ 1,
                              unitary_authority == 330 ~ 2,
                              unitary_authority == 235 ~ 3,
                              unitary_authority == 270 ~ 4,
                              unitary_authority %in% c(100, 110, 300)~5,
                              unitary_authority %in% c(120, 180, 40) ~ 6,
                              unitary_authority == 250 ~ 7,
                              unitary_authority %in% c(210, 230, 290, 400) ~8,
                              unitary_authority == 355 ~ 9,
                              unitary_authority %in% c(150, 240, 390) ~ 10,
                              unitary_authority ==130 ~ 11,
                              unitary_authority %in% c(200, 220, 260, 280, 320, 250, 380, 395) ~ 12,
                              unitary_authority %in% c(190, 310, 370) ~ 13,
                              unitary_authority == 170 ~ 14)
                              )


# Add district codes to June dataset -------------------------------------


june_geog <- june_geog %>%
  mutate(
    district = case_when(
      parish %in% 869:891 ~ 1,
      parish %in% 610:634 ~ 2,
      parish %in% c(443, 444, 457, 465, 753, 756) ~ 3,
      parish %in% 280:289 ~ 4,
      parish %in% c(767, 828, 840) ~ 5,
      parish %in% c(736:752, 757:762, 766, 768) ~ 6,
      parish %in% c(458:464, 763:765) ~ 7,
      parish %in% c(137:139, 170, 452) ~ 8,
      parish %in% c(433:437, 445:451) ~ 9,
      parish %in% c(438:442, 586) ~ 10,
      parish %in% 605:609 ~ 11,
      parish %in% c(228:230, 232:241, 587:604) ~ 12,
      parish %in% c(25:37, 49, 79:83, 220:227, 23) ~ 13,
      parish %in% c(2, 5:7, 9, 12:24, 40, 47, 50:78) ~ 14,
      parish %in% c(1, 4, 8, 10, 11, 473) ~ 15,
      parish %in% c(3 , 38:39, 41:46, 48, 466:472, 474:484) ~ 16,
      parish %in% c(84:118, 128 , 130:136) ~ 17,
      parish %in% c(119:121 , 123:127 , 129, 702) ~ 18,
      parish %in% c(122 , 485:489 , 649:658, 660:701 , 703:710 , 712) ~ 19,
      parish %in% c(403 , 407:415) ~ 20,
      parish %in% c(374:393 , 416:432) ~ 21,
      parish %in% c(394:402 , 404:406) ~ 22,
      parish %in% c(561:562 , 585 , 842:843, 846:851) ~23,
      parish %in% c(558:560 , 579:584 , 844:845) ~24,
      parish %in% c(563:566 , 569:570, 572:578) ~25,
      parish %in% c(350:373 , 571) ~ 26,
      parish %in% c(635:648) ~ 27,
      parish %in% c(265:266 , 269:271, 567:568 , 793:796, 798:805) ~28,
      parish %in% c(272 , 769:792, 797) ~ 29,
      parish %in% c(242:264 , 267:268 , 273) ~ 30,
      parish %in% c(290:294 , 659) ~ 31,
      parish %in% c(711 , 713:719 , 806:808, 818:825) ~32,
      parish %in% c(809:810 , 812:817, 841) ~ 33,
      parish %in% c(140:169 , 171:175, 276:278) ~34,
      parish %in% c(339:341 , 344 , 346:349) ~35,
      parish %in% c(549:551 , 553:555, 722) ~ 36,
      parish == 345 ~ 37,
      parish == 343 ~ 38,
      parish %in% c(342 , 552 , 826:827) ~ 39,
      parish %in% c(338 , 811) ~ 40,
      parish %in% c(548 , 556:557) ~ 41,
      parish %in% c(545:547) ~ 42,
      parish %in% c(539 , 541:543) ~ 43,
      parish %in% c(538 , 540 , 544) ~ 44,
      parish %in% c(720:721 , 723) ~ 45,
      parish %in% c(724:731) ~ 46,
      parish %in% c(732:735) ~ 47,
      parish %in% c(518:537) ~ 48,
      parish %in% c(198:203 , 216:219 , 274:275 , 279) ~ 49,
      parish %in% c(204:209 , 214:215) ~ 50,
      parish %in% c(183:192) ~ 51,
      parish %in% c(176:182 , 193:197, 210:213) ~52,
      parish %in% c(496:497 , 852:868) ~ 53,
      parish %in% c(490:495 , 498:502, 508:517) ~54,
      parish %in% c(298 , 300 , 302:308, 328:337 , 503:507) ~ 55,
      parish %in% c(295:297 , 299 , 301, 309:327) ~56,
        TRUE ~ 99
        
      )
    )


# Add region codes to June dataset ----------------------------------------

june_geog <- june_geog %>% 
  mutate(region = case_when(
    district %in% 1:3 ~ 10,
    district %in% 4:11 ~ 1,
    district %in% 12:16 ~ 2,
    district %in% 17:19 ~ 3,
    district %in% 20:22 ~ 4,
    district %in% 23:26 ~ 5,
    district %in% 27:30 ~ 6,
    district %in% 31:33 ~ 7,
    district %in% 34:52 ~ 8,
    district %in% 53:56 ~ 9,
    TRUE ~ 99
  ))


# Add county codes to June dataset -----------------------------------------


june_geog <- june_geog %>%
  mutate(
    county = case_when(
      parish %in% 1:83 ~ 66,
      parish %in%  84:136 ~ 67,
      parish %in% 137:175 ~ 68,
      parish %in% 176:219 ~ 69,
      parish %in% 220:241 ~ 70,
      parish %in% 242:273 ~ 71,
      parish %in% 274:279 ~ 72,
      parish %in% 280:289 ~ 73,
      parish %in% 290:294 ~ 74,
      parish %in% 295:337 ~ 75,
      parish %in% 338:349 ~ 76,
      parish %in% 350:373 ~ 77,
      parish %in% 374:432 ~ 78,
      parish %in% 433:465 ~ 79,
      parish %in% 466:484 ~ 80,
      parish %in% 485:489 ~ 81,
      parish %in% 490:517 ~ 82,
      parish %in% 518:557 ~ 83,
      parish %in% 558:585 ~ 84,
      parish %in% 586:604 ~ 85,
      parish %in% 605:609 ~ 86,
      parish %in% 610:634 ~ 87,
      parish %in% 635:648 ~ 88,
      parish %in% 649:719 ~ 89,
      parish %in% 720:735 ~ 90,
      parish %in% 736:768 ~ 91,
      parish %in% 769:798 ~ 92,
      parish %in% 799:805 ~ 93,
      parish %in% 806:827 ~ 95,
      parish %in% 828:840 ~ 96,
      parish %in% 841:851 ~ 97,
      parish %in% 852:868 ~ 98,
      TRUE ~ 94
    )
  )


# Add district label to June dataset --------------------------------------
june_geog <- full_join(june_geog, district, by = "district") 

# Add region label to June dataset --------------------------------------
june_geog <- full_join(june_geog, region, by = "region") 

# Add county label to June dataset --------------------------------------
june_geog <- full_join(june_geog, county, by = "county") 

# Add agric reg label to June dataset --------------------------------------
june_geog <- full_join(june_geog, agric_reg, by = "agricreg") 


disreg <- june_geog %>% filter(region == 99 | district==99)



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
  dplyr::rename(goats="item27780",
         deer = "item94",
         sheep = "item145",
         pigs = "item157", 
         poultry= "item170")


cattle <- june_geog %>%
  filter(completedata !=1 & CTS312>0) %>% 
  select(starts_with("CTS")) %>% 
  mutate(year =paste0("20", yr))


cattle_prev <- dplyr::rename_with(prev_yr, toupper) %>% 
  filter(COMPLETEDATA !=1 & CTS312>0) %>% 
  select(starts_with("CTS")) %>% 
  mutate(year =paste0("20", yr1))

cattle <- bind_rows(cattle, cattle_prev)
cattle$year <- as.numeric(cattle$year)
cattle_count <-as.data.frame(recode_response(cattle))   
cattle_summary <- as.data.frame(list(count = colSums(cattle_count, na.rm = TRUE), mean = colMeans(cattle, na.rm = TRUE) ) ) 

#20/9 - pause here: to do: all items data frame (sum and count) ...need to check all items object is relevant. might copy paste item list from main_validations  

rm()


# Save dataset ready to add SOs -------------------------------------------
# 
june_geog <-june_geog %>% dplyr::rename(unitauth = "unitary_authority")
june_temp_23 <- june_geog %>% select(parish, 
                                     holding,
                                     madeup,
                                     saf_madeup,
                                     ags_madeup, 
                                     agricreg,
                                     unitauth,
                                     county,
                                     district, 
                                     region, 
                                     NUTS2, 
                                     NUTS3, 
                                     NUTS4, 
                                    ITL2_2021_code,
                                     survtype,
                                     imptype, 
                                     survdata,
                                     land_data,
                                     other_data, 
                                     contains(c("item", "CTS")),
                                     #lsu,
                                     #LFA_area,
                                     #NLFA_area,
                                     #LFASS_area, 
                                     lfatemp, 
                                     cga, 
                                     #strata, 
                                     holdclas, 
                                     holdtype, 
                                     cc_tot_area,
                                     cc_rented_area,
                                     cc_owned_area, 
                                     #forestry_only, 
                                     landless,
                                     completedata,  
                                     saf_completed, 
                                     ) %>% 
  mutate(year = yr)


# Export june_temp to ADM -------------------------------------------------

write_dataframe_to_db(
  server = server,
  database = database,
  schema = schema,
  table_name = "JAC23_temp",
  dataframe = june_temp_23,
  append_to_existing = FALSE,
  versioned_table = FALSE,
  batch_size = 10000
)
