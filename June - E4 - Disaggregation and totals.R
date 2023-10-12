# Disaggregation and E7 of SAS code

##Written by Lucy Nevard 04/10/23
##Modified by Lucy Nevard 10/10/23



rm(list = ls())

# Before Import -----------------------------------------------------------

# Load packages and directories

library(readxl)
library(tidyverse)
library(stringr)
library(janitor)
library(data.table)
library(skimr)
library(haven)
library(RtoSQLServer)

source("item_numbers.R")


server <- "s0196a\\ADM"
database <- "RuralAndEnvironmentalScienceFarmingStatistics"
schema <- "juneagriculturalsurvey2023alpha"  # schema will change each year and need updating here. The table names can therefore stay the same 2024 onwards (will need editing below).


Code_directory <- ("//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Codeconversion_2023/2023")
sas_agstemp_path <- "//s0177a/sasdata1/ags/census/agstemp/"
sas_agscens_path <- "//s0177a/sasdata1/ags/census/agscens/"


jac <- read_table_from_db(server=server, 
                             database=database, 
                             schema=schema, 
                             table_name="JAC23_final_dataset")


jac$id<-paste0(jac$parish,"_",jac$holding)

jac$yr<-2023



# Correction just for this run - 10.10.23 - delete on any subsequent run!

correctunit<-jac %>%filter(land_data=="ags"|land_data=="both") %>% 
  dplyr::mutate(item27750=item27750/10000,
         item27755=item27755/10000,
         item85=item85/10000,
         item86=item86/10000) 

check<-correctunit %>% 
  select(item27750, item27755, item85, item86, land_data)

jac<-rows_update(jac, correctunit, by="id")


jac<-jac %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(item85=ifelse(imptype=="full", sum(item27750,item2862, na.rm=TRUE), item85),
                item86=ifelse(imptype=="full", sum(item27755,item2867, na.rm=TRUE), item86),
                item38 = sum(item41,item84,item85,item86, na.rm = TRUE),
                item40 = sum(item14, item15, item16, item18, item17, item20, item3156, item19, 
                             item23, item21, item24, item2320, item27, item28, item2034, item29, item30,
                             item31, item2059, item32, item34, item36, item2469, item2470, item35, item37, item38, na.rm=TRUE))

check<-jac %>% 
  select(item27750, item27755, item85, item86, item38, item40)


# Disaggregation ----------------------------------------------------------


# Disaggregate summary items on non-SAF returns by the proportions in their last return



land_items<-c("item27710", "item27715", "item27720", "item27725", "item27730", "item27735", "item27740", "item27750", "item27755", 
              "item16", "item18", "item17", "item20", "item19", "item23", 
              "item29", "item30", "item31", "item2059", "item32", "item34",
              "item52", "item53", "item55", "item56", "item2323", "item59", "item60", "item61", "item63", "item64", "item65", "item66",
              "item36", "item70", "item71", "item72", "item2832", "item75", 
              "item2324", "item1709", "item80", "item81", "item1710", "item82", "item83",
              "item2713", "item2858", "item2859", "item2860", "item2861", "item2714", "item2715", "item2716", "item2717", 
              "item2707", "item2863", "item2864", "item2865", "item2866", "item2708", "item2709", "item2710", "item2711")

livestock_items<-c("item27760", "item27775", "item27780", "item152", "item153", "item154", "item155", "item95", "item96", "item1712", "item1713", "item98")


# Load previous years

# previous_years <- read_table_from_db(server=server,
#                                      database=database,
#                                      schema=schema,
#                                      table_name="jac_previous_data_ten_years")


load(paste0(Code_directory, "/previous_years.rda"))

# Check glasshouse



# Filter out saf data 

land<-jac %>% 
  filter(!(land_data=="saf" | land_data =="both")) %>% 
  select(id, land_data, imptype, yr, madeup, saf_madeup, ags_madeup, land_items)


# Make individual items NA if their summary item is >0, so that the previous data can be brought forward

land<-land %>% 
  dplyr::mutate(across(c(item16, item18), ~ifelse(item27710>0 & .==0, NA, .))) %>% 
  dplyr::mutate(across(c(item17, item20), ~ifelse(item27715>0 & .==0, NA, .))) %>% 
  dplyr::mutate(across(c(item19, item23), ~ifelse(item27720>0 & .==0, NA, .))) %>% 
  dplyr::mutate(across(c(item29,item30,item31,item2059,item32,item34), ~ifelse(item27725>0 & .==0, NA, .))) %>% 
  dplyr::mutate(across(c(item52,item53,item55,item56,item2323,item59,item60,item61,item63,item64,item65,item66), ~ifelse(item27730>0 & .==0, NA, .))) %>% 
  dplyr::mutate(across(c(item36,item70,item71,item72,item2832,item75), ~ifelse(item27735>0 & .==0, NA, .))) %>% 
  dplyr::mutate(across(c(item2324,item1709,item80,item81,item1710,item82,item83), ~ifelse(item27740>0 & .==0, NA, .))) %>% 
  dplyr::mutate(across(c(item2713, item2858, item2859, item2860, item2861, item2714, item2715, item2716, item2717), ~ifelse(item27750>0 & .==0, NA, .))) %>% 
  dplyr::mutate(across(c(item2707, item2863, item2864, item2865, item2866, item2708, item2709, item2710, item2711), ~ifelse(item27755>0 & .==0, NA, .)))

check<-land %>% 
  select(id, item27730,item52,item53,item55,item56,item2323,item59,item60,item61,item63,item64,item65,item66)



previous_years_land<-previous_years %>% 
  select(any_of(names(land)))

previous_years_land<-subset(previous_years_land, id %in% land$id)

# Make zeroes NA in previous years so that the last non-zero (as opposed to non-NA) is carried forward

previous_years_land<-previous_years_land %>% 
  dplyr::mutate(across(c(item16, item18, item17, item20, item19, item23, 
                  item29,item30,item31,item2059,item32,item34,
                  item52,item53,item55,item56,item2323,item59,item60,item61,item63,item64,item65,item66,
                  item36,item70,item71,item72,item2832,item75, 
                  item2324,item1709,item80,item81,item1710,item82,item83,
                  item2713, item2858, item2859, item2860, item2861, item2714, item2715, item2716, item2717, 
                  item2707, item2863, item2864, item2865, item2866, item2708, item2709, item2710, item2711),
                ~ifelse(.==0, NA, .)))


# Combine datasets

land_full<-bind_rows(land,previous_years_land)

land_full<-land_full %>% 
  filter(!is.na(yr))

land_complete<-land_full

# keep only holdings where saf_madeup<10 or ags_madeup<10 in 2021. Change this so that those with items>0 are kept, currently some with madeup=NA in 2021 are lost here. 

land_full<-land_full %>% 
  group_by (id) %>% 
  filter(any(saf_madeup<10 & yr==2021 |ags_madeup<10 & yr==2021))


# Overwrite with the last returned data for each holding


land_full<- land_full[order(as.numeric(as.character(land_full$yr))), ]

land_rf<- land_full %>% 
  group_by (id) %>% 
  dplyr::mutate(across(c(item16, item18, item17, item20, item19, item23, 
                         item29,item30,item31,item2059,item32,item34,
                         item52,item53,item55,item56,item2323,item59,item60,item61,item63,item64,item65,item66,
                         item36,item70,item71,item72,item2832,item75, 
                         item2324,item1709,item80,item81,item1710,item82,item83,
                         item2713, item2858, item2859, item2860, item2861, item2714, item2715, item2716, item2717, 
                         item2707, item2863, item2864, item2865, item2866, item2708, item2709, item2710, item2711), 
                       ~ifelse(is.na(.[yr==2023]) & yr==2023, zoo::na.locf(., na.rm = FALSE), .)))



# Keep 2023 data

land_2023<-land_rf %>% 
  filter(yr==2023) %>% 
  ungroup()

# Create totals for last returned

land_split<-land_2023 %>% 
  rowwise() %>% 
  dplyr::mutate(
    barley_27710=sum(item16,item18, na.rm=TRUE),
    oats_27715=sum(item17,item20, na.rm=TRUE),
    osr_27720=sum(item19,item23, na.rm=TRUE),
    stockfeed_27725=sum(item29,item30,item31,item2059,item32,item34,na.rm=TRUE),
    veginopen_27730=sum(item52,item53,item55,item56,item2323,item59,item60,item61,item63,item64,item65,item66, na.rm=TRUE),
    fruitinopen_27735=sum(item36,item70,item71,item72,item2832,item75, na.rm=TRUE),
    nursery_27740=sum(item2324,item1709,item80,item81,item1710,item82,item83, na.rm=TRUE),
    opensoil_27750=sum(item2713, item2858, item2859, item2860, item2861, item2714, item2715, item2716, item2717, na.rm=TRUE),
    solidfloor_27755=sum(item2707, item2863, item2864, item2865, item2866, item2708, item2709, item2710, item2711, na.rm = TRUE)) %>% 
  dplyr::mutate(
    prop_16=item16/barley_27710,
    prop_18=item18/barley_27710,
    prop_17=item17/oats_27715,
    prop_20=item20/oats_27715,
    prop_19=item19/osr_27720,
    prop_23=item23/osr_27720,
    prop_29=item29/stockfeed_27725,
    prop_30=item30/stockfeed_27725,
    prop_31=item31/stockfeed_27725,
    prop_2059=item2059/stockfeed_27725,
    prop_32=item32/stockfeed_27725,
    prop_34=item34/stockfeed_27725,
    prop_52=item52/veginopen_27730,
    prop_53=item53/veginopen_27730,
    prop_55=item55/veginopen_27730,
    prop_56=item56/veginopen_27730,
    prop_2323=item2323/veginopen_27730,
    prop_59=item59/veginopen_27730,
    prop_60=item60/veginopen_27730,
    prop_61=item61/veginopen_27730,
    prop_63=item63/veginopen_27730,
    prop_64=item64/veginopen_27730,
    prop_65=item65/veginopen_27730,
    prop_66=item66/veginopen_27730,
    prop_36=item36/fruitinopen_27735,
    prop_70=item70/fruitinopen_27735,
    prop_71=item71/fruitinopen_27735,
    prop_72=item72/fruitinopen_27735,
    prop_2832=item2832/fruitinopen_27735,
    prop_75=item75/fruitinopen_27735,
    prop_2324=item2324/nursery_27740,
    prop_1709=item1709/nursery_27740,
    prop_80=item80/nursery_27740,
    prop_81=item81/nursery_27740,
    prop_1710=item1710/nursery_27740,
    prop_82=item82/nursery_27740,
    prop_83=item83/nursery_27740,
    prop_2713=item2713/opensoil_27750,
    prop_2858=item2858/opensoil_27750,
    prop_2859=item2859/opensoil_27750,
    prop_2860=item2860/opensoil_27750,
    prop_2861=item2861/opensoil_27750,
    prop_2714=item2714/opensoil_27750,
    prop_2715=item2715/opensoil_27750,
    prop_2716=item2716/opensoil_27750,
    prop_2717=item2717/opensoil_27750,
    prop_2707=item2707/solidfloor_27755,
    prop_2863=item2863/solidfloor_27755,
    prop_2864=item2864/solidfloor_27755,
    prop_2865=item2865/solidfloor_27755,
    prop_2866=item2866/solidfloor_27755,
    prop_2708=item2708/solidfloor_27755,
    prop_2709=item2709/solidfloor_27755,
    prop_2710=item2710/solidfloor_27755,
    prop_2711=item2711/solidfloor_27755) %>% 
  dplyr::mutate_all(function(x) ifelse(is.nan(x), 0, x)) %>% 
  dplyr::mutate(
    item16=prop_16*item27710,
    item18=prop_18*item27710,
    item17=prop_17*item27715,
    item20=prop_20*item27715,
    item19=prop_19*item27720,
    item23=prop_23*item27720,
    item29=prop_29*item27725,
    item30=prop_30*item27725,
    item31=prop_31*item27725,
    item2059=prop_2059*item27725,
    item32=prop_32*item27725,
    item34=prop_34*item27725,
    item52=prop_52*item27730,
    item53=prop_53*item27730,
    item55=prop_55*item27730,
    item56=prop_56*item27730,
    item2323=prop_2323*item27730,
    item59=prop_59*item27730,
    item60=prop_60*item27730,
    item61=prop_61*item27730,
    item63=prop_63*item27730,
    item64=prop_64*item27730,
    item65=prop_65*item27730,
    item66=prop_66*item27730,
    item36=prop_36*item27735,
    item70=prop_70*item27735,
    item71=prop_71*item27735,
    item72=prop_72*item27735,
    item2832=prop_2832*item27735,
    item75=prop_75*item27735,
    item2324=prop_2324*item27740,
    item1709=prop_1709*item27740,
    item80=prop_80*item27740,
    item81=prop_81*item27740,
    item1710=prop_1710*item27740,
    item82=prop_82*item27740,
    item83=prop_83*item27740,
    item2713=prop_2713*item27750,
    item2858=prop_2858*item27750,
    item2859=prop_2859*item27750,
    item2860=prop_2860*item27750,
    item2861=prop_2861*item27750,
    item2714=prop_2714*item27750,
    item2715=prop_2715*item27750,
    item2716=prop_2716*item27750,
    item2717=prop_2717*item27750,
    item2707=prop_2707*item27755,
    item2863=prop_2863*item27755,
    item2864=prop_2864*item27755,
    item2865=prop_2865*item27755,
    item2866=prop_2866*item27755,
    item2708=prop_2708*item27755,
    item2709=prop_2709*item27755,
    item2710=prop_2710*item27755,
    item2711=prop_2711*item27755
  ) %>% 
  ungroup()


land_final <- land_split %>% 
  select(id, land_data, imptype, madeup, saf_madeup, ags_madeup, land_items)

check2<-land_final %>% 
  select(id, item27710, item16, item18, item27730,item52,item53,item55,item56,item2323,item59,item60,item61,item63,item64,item65,item66)


checkholdingspost<-land_final %>% 
  select(id, item27710, item16, item18) %>% 
  filter(id=="666_78"|id=="816_58" |id=="417_8")


non_disag<-land_final %>% 
  filter(item27710>0 &(item16+item18==0))
  

# Livestock items 


livestock<-jac %>% 
  select(id, land_data, imptype, yr, madeup, saf_madeup, ags_madeup, any_of(livestock_items))


# Make individual items NA if their summary item is >0, so that the previous data can be brought forward

livestock<-livestock %>% 
  dplyr::mutate(across(c(item95,item96), ~ifelse(item27775>0 & .==0, NA, .))) %>% 
  dplyr::mutate(across(c(item1712,item1713,item98), ~ifelse(item27780>0 & .==0, NA, .))) %>% 
  dplyr::mutate(across(c(item152, item153, item154, item155), ~ifelse(item27760>0 & .==0, NA, .)))

check<-livestock %>% 
  select(id, item27760,item152, item153, item154, item155)


previous_years__livestock<-previous_years %>% 
  select(any_of(names(livestock)))

previous_years_livestock<-subset(previous_years__livestock, id %in% livestock$id)

# Make zeroes NA in previous years so that the last non-zero (as opposed to non-NA) is carried forward

previous_years_livestock<-previous_years_livestock %>% 
  dplyr::mutate(across(c(item152, item153, item154, item155, item95, item96, item1712,item1713,item98),
                ~ifelse(.==0, NA, .)))

# Combine datasets

livestock_full<-bind_rows(livestock, previous_years_livestock)

livestock_full<-livestock_full %>% 
  filter(!is.na(yr))

# keep only holdings where ags_madeup<10 in 2021

livestock_full<-livestock_full %>% 
  group_by (id) %>% 
  filter(any(ags_madeup<10 & yr==2021))


# overwrite with previous return


livestock_full<- livestock_full[order(as.numeric(as.character(livestock_full$yr))), ]

livestock_rf <- livestock_full %>% 
  group_by (id) %>% 
  dplyr::mutate(across(c(item152, item153, item154, item155, item95, item96, item1712,item1713,item98), 
                       ~ifelse(is.na(.[yr==2023]) & yr==2023, zoo::na.locf(., na.rm = FALSE), .))) 


# Keep 2023 data

livestock_2023<-livestock_rf %>% 
  filter(yr==2023)

# Create totals

livestock_final<-livestock_2023 %>% 
  rowwise() %>% 
  dplyr::mutate(horses_27775=sum(item95,item96, na.rm=TRUE),
         goats_27780=sum(item1712,item1713,item98, na.rm=TRUE),
         fat_pigs27760=sum(item152, item153, item154, item155)) %>% 
  dplyr::mutate(prop_95=item95/horses_27775,
         prop_96=item96/horses_27775,
         prop_1712=item1712/goats_27780,
         prop_1713=item1713/goats_27780,
         prop_98=item98/goats_27780,
         prop_152=item152/fat_pigs27760,
         prop_153=item153/fat_pigs27760,
         prop_154=item154/fat_pigs27760,
         prop_155=item155/fat_pigs27760) %>% 
  dplyr::mutate_all(function(x) ifelse(is.nan(x), 0, x)) %>% 
  dplyr::mutate(item95=prop_95*item27775,
         item96=prop_96*item27775,
         item1712=prop_1712*item27780,
         item1713=prop_1713*item27780,
         item98=prop_98*item27780,
         item152=prop_152*item27760,
         item153=prop_153*item27760,
         item154=prop_154*item27760,
         item155=prop_155*item27760) %>% 
  ungroup()


livestock_final<-livestock_final %>% 
  select(id, livestock_items)

# Overwrite final dataset

jac$id<-paste0(jac$parish,"_",jac$holding)


land_final<-dplyr::ungroup(land_final)
land_final<-as_data_frame(land_final)

jac<-jac %>% 
  dplyr::mutate(across(starts_with('item')& !starts_with("item185") &!starts_with("item186"), as.numeric))

jac_disag<-rows_update(jac, land_final, by="id")

jac_disag<-rows_update(jac_disag, livestock_final, by="id")


checkbefore<-jac %>% 
  select(parish, holding, imptype, land_data, any_of(c(land_items, livestock_items)))

checkafter<-jac_disag %>% 
  select(parish, holding, imptype, land_data, any_of(c(land_items, livestock_items)))


# Make summary items same as individual items



jac_disag<-clean_names(jac_disag)

str(jac_disag)

jac_disag<-jac_disag %>% 
  dplyr::mutate(item27710=ifelse(item27710==0&(item16+item18)>0, (item16+item18), item27710),
         item27715=ifelse(item27715==0&(item17+item20)>0, (item17+item20), item27715),
         item27720=ifelse(item27720==0&(item19+item23)>0, (item19+item23),item27720),
         item27725=ifelse(item27725==0&(item29+item30+item31+item2059+item32+item34)>0, (item29+item30+item31+item2059+item32+item34),item27725),
         item27730=ifelse(item27730==0&(item52+item53+item55+item56+item2323+item59+item60+item61+item63+item64+item65+item66)>0, (item52+item53+item55+item56+item2323+item59+item60+item61+item63+item64+item65+item66),item27730),
         item27735=ifelse(item27735==0&(item36+item70+item71+item72+item2832+item75)>0, (item36+item70+item71+item72+item2832+item75),item27735),
         item27740=ifelse(item27740==0&(item2324+item1709+item80+item81+item1710+item82+item83)>0, (item2324+item1709+item80+item81+item1710+item82+item83),item27740),
         item27750=ifelse(item27750==0&(item2713+ item2858+ item2859+ item2860+ item2861+ item2714+ item2715+ item2716+ item2717)>0, (item2713+ item2858+ item2859+ item2860+ item2861+ item2714+ item2715+ item2716+ item2717),item27750),
         item27755=ifelse(item27755==0&(item2707+ item2863+ item2864+ item2865+ item2866+ item2708+ item2709+ item2710+ item2711)>0, (item2707+ item2863+ item2864+ item2865+ item2866+ item2708+ item2709+ item2710+ item2711),item27755))



jac_disag<-jac_disag %>% 
  dplyr::mutate(item27710=ifelse(land_data=="saf"|land_data=="both", (item16+item18), item27710),
         item27715=ifelse(land_data=="saf"|land_data=="both", (item17+item20), item27715),
         item27720=ifelse(land_data=="saf"|land_data=="both", (item19+item23),item27720),
         item27725=ifelse(land_data=="saf"|land_data=="both", (item29+item30+item31+item2059+item32+item34),item27725),
         item27730=ifelse(land_data=="saf"|land_data=="both", (item52+item53+item55+item56+item2323+item59+item60+item61+item63+item64+item65+item66),item27730),
         item27735=ifelse(land_data=="saf"|land_data=="both", (item36+item70+item71+item72+item2832+item75),item27735),
         item27740=ifelse(land_data=="saf"|land_data=="both",(item2324+item1709+item80+item81+item1710+item82+item83),item27740),
         item27750=ifelse(land_data=="saf"|land_data=="both", (item2713+ item2858+ item2859+ item2860+ item2861+ item2714+ item2715+ item2716+ item2717),item27750),
         item27755=ifelse(land_data=="saf"|land_data=="both", (item2707+ item2863+ item2864+ item2865+ item2866+ item2708+ item2709+ item2710+ item2711),item27755))




# Round everything here - all livestock and workforce items to whole numbers and land items to 2 dp

jac_disag<-jac_disag %>% 
  dplyr::mutate(across(any_of(c(all_sheep, all_pig, all_poultry, all_other_livestock, workforce)), round_half_up, 0))

jac_disag<-jac_disag %>% 
  dplyr::mutate(across(any_of(c("item20026","item11","item12","item50", land_items, all_crops, all_grass, all_other_land)), round_half_up, 2))



jac_disagbeforecorrections<-jac_disag # save this file


# non_disagbarley<-jac_disag %>% 
#   select(id, item27710, imptype, item16, item18) %>% 
#   filter(item27710>0 & (sum(item16,item18, na.rm=TRUE)==0 |is.na(item16)&is.na(item18)))


jac_disag<-ungroup(jac_disag)
# 
# jac_disag<-jac_disag %>% 
#   dplyr::mutate(prop_item16=sum(item16, na.rm=TRUE)/sum(item27710, na.rm=TRUE),
#                 prop_item18=sum(item18, na.rm=TRUE)/sum(item27710, na.rm=TRUE),
#                 prop_item17=sum(item17, na.rm=TRUE)/sum(item27710, na.rm=TRUE
#                 
#                                                         # complete these for all summary items
               
                



# Make totals based on E7 of SAS code -------------------------------------------------------------


# correct two holdings - clearly decimal place errors


checkfruitandothercrops<-jac_disag %>% 
  select(parish, holding, land_data, item75, item2714,item2708, item2713, item2707, item34, item2059, item38, item41, item84, item85, item86)


checkholdingsfruit<-previous_years %>% 
  select(parish, holding,yr,item75,item2713,item2707, item2036, item2714) %>% 
  filter(parish==674 & holding ==44 | parish==674 & holding==17 |parish==94 &holding==47 |parish==472&holding==121)  

checkholdingsothercrops<-previous_years %>% 
  select(parish, holding,yr,item75,item2713,item2707, item2036, item2714) %>% 
  filter(parish==674 & holding ==44 | parish==674 & holding==17 |parish==94 &holding==47 |parish==472&holding==121)  



jac_disag<-jac_disag %>% 
  dplyr::mutate(
    item2714=ifelse(parish==674 & holding ==44, 3.4, item2714),
    item2714=ifelse(parish==674 & holding==17, 0.4, item2714),
    item2707=ifelse(parish==472 & holding==121, 0.021, item2707),
    item2036=ifelse(parish==94 & holding==47, 0.204, item2714)
  )

occandocc2<-c("item177", "item178", "item179", "item182", "item183", "item184","item2566", "item2567","item2877", "item2878", "item3056", "item3057")

jac_disag<-jac_disag %>% 
  dplyr::mutate(across(any_of(occandocc2), ~ifelse (.>0, 1, .)))



checkitem50<-jac_disag %>%
  select(parish, holding, imptype, item85, item86, item38, item41, item84, item40,item14, item15, item16, item18, item17, item20, item3156, item19, 
         item23, item21, item24, item2320, item27, item28, item2034, item29, item30,
         item31, item2059, item32, item34, item36, item2469, item2470, item35, item37, item38, 
         item46, item47, item48, item49, item50,item2321, item2322)

jac_disag<-jac_disag %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(
    item46 = sum(item2321, item2322, item40, na.rm=TRUE),
    item50 = sum(item46, item47, item48, item49, na.rm=TRUE))


checkglasshouse<-jac_disag %>% 
  select(parish, holding, imptype, land_data, item85, item86, 
         item27750,item2862, item2713, item2858, item2859, item2860, item2861, item2714, item2715, item2716, item2717,
         item27755, item2867,item2707, item2863, item2864, item2865, item2866, item2708, item2709, item2710, item2711) %>% 
  filter(item85+item86==0&(land_data=="both"|land_data=="saf")) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(
         item85=sum(item2862, item2713, item2858, item2859, item2860, item2861, item2714, item2715, item2716, item2717,na.rm=TRUE),
         item86=sum(item2867,item2707, item2863, item2864, item2865, item2866, item2708, item2709, item2710, item2711,na.rm=TRUE),
         item27750=sum(item2713, item2858, item2859, item2860, item2861, item2714, item2715, item2716, item2717,na.rm=TRUE),
         item27755=sum(item2707, item2863, item2864, item2865, item2866, item2708, item2709, item2710, item2711,na.rm=TRUE))

jac_disag<-rows_update(jac_disag, checkglasshouse, by=c("parish","holding"))



checkglasshouse<-jac_disag %>% 
  select(parish, holding, imptype, land_data, item85, item86, 
         item27750,item2862, item2713, item2858, item2859, item2860, item2861, item2714, item2715, item2716, item2717,
         item27755, item2867,item2707, item2863, item2864, item2865, item2866, item2708, item2709, item2710, item2711)
                
# make items 

jac_disag<-jac_disag %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(item87=sum(item2713,item2707, na.rm=TRUE), #bear in mind 2713 isn't on SAF so could be an underestimate (made from disaggregating ags-only and imputed data)
         item39=sum(item2469,item2470, na.rm=TRUE),
         item6000=sum(item2861, item2866, na.rm=TRUE),
         item2036=sum(item2714, item2708,na.rm=TRUE),
         item2037=sum(item2715, item2709, na.rm=TRUE),
         item1711=sum(item2716, item2710, na.rm=TRUE),
         item1943=sum(item2717, item2711, na.rm=TRUE),
         lambs=item144,
         ewes=item139,
         pigs20=item27770,
         pigs50=sum(item155,item27765, na.rm=TRUE), # item155 is disaggregated from 27760 but weaners are a separate category 27765
         pigs80=item154,
         tpigsbig=sum(item153,item152, na.rm=TRUE),
         pigsbig=tpigsbig,
         tothpigs=sum(item157-sum(item27760, item27765, item27770, na.rm=TRUE), na.rm=TRUE),
         othpigs=tothpigs,
         broilers=item164,
         tlayers=sum(item158,item159, na.rm=TRUE),
         layers=tlayers,
         peas=item52,
         cala=item60,
         turswe=item56,
         carrots=item63,
         #tveg=sum(item68,item36,item37, na.rm=TRUE) - sum(item52,item60,item56,item63, na.rm=TRUE),  # not sure this makes sense, produces some negative numbers...
         #veg=tveg,
         tothcer=sum(item14,item15,item17,item20, na.rm=TRUE), # no item22
         othcer=tothcer)


jac_disag<-jac_disag %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(
    otherfr_open=sum(item75,item72, na.rm=TRUE), # note from SAS: include blackcurrants in open (blackcurrants under cover are disclosive)
    otherfr_covered=sum(item2036,item6000, na.rm=TRUE),
    strawb = sum(item2556,item70, na.rm=TRUE),
    raspb = sum(item2557,item71, na.rm=TRUE),
    blackc = sum(item6000,item72, na.rm=TRUE),
    blueb = sum(item2836,item2832, na.rm=TRUE),
    tomato = sum(item87, na.rm=TRUE),
    otherfr = sum(item75,item2036, na.rm=TRUE),
    fruit = sum(strawb,raspb,blackc,blueb,tomato,otherfr, na.rm=TRUE),  # SAS code also includes mixedfr here but can't see how it's made. Mixed fruit is item75 which is already in othfr
    fewes=sum(ewes,lambs, na.rm=TRUE), # note didn't make fsheep=sum(sheep,fewes), not in the tables
    fpigs50=sum(pigs50,pigs20, na.rm=TRUE),
    fpigs80=sum(fpigs50,pigs80, na.rm=TRUE),
    fpigsbig=sum(pigsbig,fpigs80, na.rm=TRUE),
    fothpigs=sum(othpigs,fpigsbig,na.rm=TRUE),
    flayers=sum(layers,broilers, na.rm=TRUE),  # note didn't make fothpoul=othpoul+flayers because the SAS code didn't make sense to me and it's not in the tables anyway
    fcala=sum(cala,peas, na.rm=TRUE),
    fturswe=sum(turswe,fcala, na.rm=TRUE),
    fcarrots=sum(carrots,fturswe, na.rm=TRUE),
    fveg=sum(veg,fcarrots, na.rm=TRUE),
    fothcer=sum(othcer, item27710, na.rm=TRUE),
    fal5=sum(item2469,item2470, na.rm=TRUE),
    othcrops=sum(item34,item2059, na.rm=TRUE),
    othcrops2=sum(item38, na.rm=TRUE),  # in SAS this includes item2319 but it's not on the dataset
    othcrops3=sum(item38,-sum(item87,item2556,item2557,item2836,item6000,item2036, na.rm=TRUE), na.rm=TRUE),
    fodder=sum(item34,item2059,item2034,item29,item30,item31,item32, na.rm=TRUE),
    othveg=sum(item55,item2323,item59,item64,item65,item66, na.rm=TRUE),
    bigpigs=sum(item152,item153, na.rm=TRUE),
    layers=sum(item158,item159, na.rm=TRUE),
    breeders=sum(item160,item162, na.rm=TRUE),
    breefowl=sum(item160,item162,item163, na.rm=TRUE),
    eggfowl=sum(item158,item159,item161, na.rm=TRUE),
    othpoul=sum(item2038,item2039,item167, na.rm=TRUE),
    breepigs=sum(item146,item147,item148, na.rm=TRUE),
    othpigs=sum(item27760, item27765, item27770, na.rm=TRUE), # why does SAS code assign othpigs twice, using different items...
    goats=item27780,
    grass=sum(item2321,item2322, na.rm=TRUE),
    potatoes=sum(item24,item2320, na.rm=TRUE),
    toats=item27715,
    tglasshouse = sum(item85,item86, na.rm=TRUE),
    cereal=sum(item14,item15,item16, item18, item17, item20, item3156, na.rm=TRUE),
    osr=item27720,
    oilseeds=sum(item27720, item21, na.rm=TRUE),
    combine=sum(item14,item15,item27710, item27715, item27720,
                item27,item28,item21, item3156, na.rm=TRUE),
    horses=item27775,
    othsheep=sum(item141,item143, na.rm=TRUE),
    ftmale=sum(item1715,item1716,item1714, na.rm=TRUE),
    ftfemale=sum(item192,item193,item1717, na.rm=TRUE),
    ptmale=sum(item194,item195,item1718, na.rm=TRUE),
    ptfemale=sum(item196,item197,item1719, na.rm=TRUE),
    ftregular=sum(ftfemale,ftmale, na.rm=TRUE),
    ptregular=sum(ptfemale,ptmale, na.rm=TRUE),
    regstaff=sum(item1715,item1716,item1714,item192,item193,item1717
                 ,item194,item195,item1718,item196,item197,item1719, na.rm=TRUE),
    casual=sum(item198,item199, na.rm=TRUE),
    occ=sum(item177,item178,item179, na.rm=TRUE),
    sp=sum(item182,item183,item184, na.rm=TRUE),
    occsp=sum(item177,item178,item179,item182,item183,item184, na.rm=TRUE),
    workforce=sum(item200,occ, sp, na.rm=TRUE),
    camelids=sum(item2472, item2473, item2474, na.rm=TRUE),
    bcat=sum(cts303,cts305,cts307, na.rm=TRUE),
    dcat=sum(cts304,cts306,cts308, na.rm=TRUE),
    mcat=sum(cts310,cts311, na.rm=TRUE),
    calves=sum(cts301,cts302,cts309, na.rm=TRUE)
  )



land_use_items<-c(land_items,all_crops, "cga", "item24","item2320","item41","item38","item39", "item2469","item2470","item40",all_grass,"item46",all_other_land,
                  "item70", "item71", "item72", "item2832","item76","item85","item86","item2556","item2557","item2836","item6000","item2036","item2037","item1711","item1943","item6001")
area_workforce_items<-c("item20026","item11","item12","item50",workforce,other_labour,"item177","item178", "item179", "item2566","item182","item183","item184", "item2567")

livestock_all_items<-c(all_cattle, all_sheep,all_pig, "item152","item153","item154","item155", all_poultry,all_other_livestock, "item27775","item27780")

added_items<-c("item87","item39","item6000","item2036","item2037", "item1711", "item1943", "lambs", "ewes",
               "pigs20","pigs50",   "pigs80",   "tpigsbig", "pigsbig",  "tothpigs", "othpigs",  "broilers", "tlayers", "layers", 
               "peas", "cala",     "turswe", "carrots", "tveg", "veg",  "tothcer",  "othcer","otherfr_open","otherfr_covered",
               "strawb","raspb","blackc","blueb","tomato","otherfr","fruit","fewes","fpigs50","fpigs80","fpigsbig","fothpigs",
               "fothcer","fal5" ,"othcrops","othcrops2","othcrops3","fodder","othveg", "bigpigs","breeders", "breefowl","eggfowl", 
               "othpoul","breepigs","goats","grass","potatoes","toats","tglasshouse","cereal","osr","oilseeds","combine","horses",
               "othsheep","ftmale","ftfemale","ptmale","ptfemale","ftregular","ptregular","regstaff","casual","occ","sp",
               "occsp","workforce","camelids","bcat" ,"dcat","mcat","calves")

# check new items make sense

checkaddeditems<-jac_disag %>% 
  select(parish, holding, added_items)


# free unused R memory
gc()


# make item totals

# make sure it's ungrouped

jac_disag<-ungroup(jac_disag)

str(jac_disag)



jac_totals<-jac_disag %>% 
  dplyr::mutate(across(all_of(c(land_use_items,area_workforce_items,livestock_all_items, added_items)), ~sum(., na.rm=TRUE), .names="total{.col}", .keep="unused"))

jac_totals<-jac_totals %>% 
  dplyr::rename_with(~str_remove(.,"item"),starts_with("total")) %>% 
  dplyr::rename_with(~str_remove(.,"cts"),starts_with("total")) 


# check totals make sense

checktotals<-jac_totals %>% 
  select(starts_with("total"))


         
         
# SAS code also includes src = sum(total2471), item2471 not on dataset and don't know what that is. 
# Glasshouse items – some items are not on SAF 
# Didn’t make total of item2319 aromatic plants because it’s not on the dataset

# Make dataframe corresponding to tables

tables_nice<-jac_totals %>% 
  select(starts_with("total")) %>% 
  filter(row_number()==1) %>% 
  mutate(
    wheat=total14,
    triticale=total15,
    winterbarley=total16, 
    springbarley=total18,
    winteroats=total17,
    springoats=total20,
    rye=total3156,
    cereal=totalcereal,
    winterosr=total19,
    springosr=total23,
    linseed=total21,
    oilseeds=totaloilseeds,
    proteinpeas=total28,
    fieldbeans=total27,
    combine=totalcombine,
    seedpotato=total24,
    warepotato=total2320,
    allpotato=totalpotatoes,
    turnipswede=total29,
    kalecabbage=total30,
    maize=total2059,
    rape=total31,
    fodderbeet=total32,
    lupins=total2034,
    othercropsstockfeed=total34,
    fodder=totalfodder,
    veghuman=total27730,
    orchardfruit=total36,
    softfruit=totalfruit,
    othercrops=totalothcrops3, 
    fallowunder5=total2469,
    fallowover5=total2470,
    allfallow=total39,
    cropsfallowsetaside=total40,
    grassunder5=total2321,
    grassover5=total2322,
    allgrass=totalgrass,
    cropsgrass=total46,
    roughgrazing=total47,
    otherincwoodland=total49+total48,
    agriculturalarea=total50,
    commongrazing=totalcga,  # this isn't in SAS so I made it using the only common grazing variable
    peascanning=total52,
    beanscanning=total53,
    turnipswedehuman=total56,
    calabrese=total60,
    cauliflowerbroccoli=total61,
    carrots=total63,
    otherveg=totalothveg,
    bulbsflowers=total27740,
    strawberries=total70,
    raspberries=total71,
    blueberries=total2832,
    otherfruitopen=totalotherfr_open,
    totalsoftfruitopen=total76,
    tomatoesundercover=total87,
    strawbundercover=total2556,
    raspbundercover=total2557,
    bluebundercover=total2836,
    otherfruitundercover=totalotherfr_covered,
    vegundercover=total2037,
    beddingpotundercover=total1711,
    hardyundercover=total1943,
    unusedundercover=total6001,
    alltomato=totaltomato,
    allstrawb=totalstrawb,
    allraspb=totalraspb,
    allblackc=totalblackc,
    allblueb=totalblueb,
    allotherfruit=totalotherfr,
    allsoftfruit=totalfruit,
    plastic=total85,
    glass=total86,
    allglasshouse=totaltglasshouse,
    femaledairy1to2=total304,
    femaledairy2off=total306,
    femaledairy2nooff=total308,
    alldairy=totaldcat,
    femalebeef1to2=total303,
    femalebeef2off=total305,
    femalebee2nooff=total307,
    allbeef=totalbcat,
    male1to2=total310,
    male2=total311,
    allmale=totalmcat,
    femaledairyunder1=total302,
    femalebeefunder1=total301,
    maleunder1=total309,
    allcalves=totalcalves,
    allcattle=total312,
    ewes=total139,
    rams=total140,
    othersheep1=total141,
    othersheep=total143,
    allothersheep=totalothsheep,
    lambs=total144,
    allsheep=total145,
    sowsinpig=total146,
    gltsinpig=total147,
    othersows=total148,
    allbreedingpigs=totalbreepigs,
    barrensows=total149,
    gilts50=total150,
    boars=total151,
    pigs80=totalpigsbig,
    pigs50to80=total154,
    pigsunder50=totalpigs50+totalpigs20,
    allotherpigs=totalothpigs,
    allpigs=total157,
    pulletshenslaying=totallayers,
    pulletsrearedlaying=total161, 
    allfowlseggs=totaleggfowl,
    breedinghens=totalbreeders,
    cocks=total163,
    allbreedingfowl=totalbreefowl,
    broilers=total164,
    turkeys=total1708,
    otherpoultry=totalothpoul,
    allpoultry=total170,
    deer=total94, 
    horses=totalhorses,
    donkeys=total2868,
    goats=totalgoats,
    camelids=totalcamelids,
    beehives=total2826,
    occ1occ2ft=total177+total182,
    occ1occ2ht=total178+total183,
    occ1occ2lht=total179+total184,
    allwork=totaloccsp,
    nowork=total2566+total2567,
    ftmaleshired=total1715,
    ftmalesfamily=total1716,
    ftmalespartners=total1714,
    allftmales=totalftmale,
    ftfemaleshired=total192,
    ftfemalesfamily=total193,
    ftfemalespartners=total1717,
    allftfemales=totalftfemale,
    allft=totalftregular,
    ptmaleshired=total194,
    ptmalesfamily=total195, 
    ptmalespartners=total1718,
    allptmales=totalptmale,
    ptfemaleshired=total196,
    ptfemalesfamily=total197, 
    ptfemalespartners=total1719,
    allptfemales=totalptfemale,
    allpt=totalptregular,
    allregstaff=totalregstaff,
    malescasual=total198,
    femalescasual=total199,
    allcasual=totalcasual,
    workforce=totalworkforce,
    european=total2536,
    noneuropean=total2511,
    rented=total20026,
    owned=total11,
    areaoccupied=total12
   
 
  ) %>% 
  select(-starts_with("total"))

checkbroiler<-jac_totals %>% 
  select(parish, holding, item164) %>% 
  select(parish, holding, item164) %>% 
  mutate(totalbroiler=sum(item164, na.rm=TRUE))


checkbroiler2021<-previous_years %>% 
  filter(yr==2021 & ags_madeup==0) %>% 
  select(parish, holding, item164, ags_madeup) %>% 
  mutate(totalbroiler=sum(item164, na.rm=TRUE))


checkbroiler2021<-previous_years %>% 
  filter(yr==2021 & ags_madeup>0) %>% 
  select(parish, holding, item164, ags_madeup) %>% 
mutate(totalbroiler=sum(item164, na.rm=TRUE))


checkoccsp<-jac_totals %>% 
  select(parish, holding, land_data, item177, item178, item179, item182, item183, item184, item194, item195)

checkoccspextract<-df_nonSAF %>% 
  select(parish, holding, item177, item178, item179, item182, item183, item184, item194, item195)

checkcg<-previous_years %>% 
  filter(yr==2021) %>% 
  select(parish, holding, land_data, cgarea)

checkoccsprealprev<-previous_years %>% 
  filter(yr==2021) %>% 
  select(parish, holding, ags_madeup, survtype, item177, item178, item179, item182, item183, item184, item2566, item2567) %>% 
  filter(ags_madeup==0) %>% 
  dplyr::mutate(across(any_of(occandocc2), ~ifelse (.>0, 1, .))) %>% 
  rowwise() %>% 
  mutate(occsp=sum(item177, item178, item179, item182, item183, item184, na.rm=TRUE)) %>% 
  mutate(nowork=sum(item2566, item2567, na.rm=TRUE))

sum(checkoccsprealprev$occsp, na.rm=TRUE)
sum(checkoccsprealprev$nowork, na.rm=TRUE)



checkoccsprfprev<-previous_years %>% 
  filter(yr==2021) %>% 
  select(parish, holding, ags_madeup, survtype, item177, item178, item179, item182, item183, item184, item2566, item2567) %>% 
  filter(ags_madeup>0) %>% 
  dplyr::mutate(across(any_of(occandocc2), ~ifelse (.>0, 1, .))) %>% 
  rowwise() %>% 
  mutate(occsp=sum(item177, item178, item179, item182, item183, item184, na.rm=TRUE)) %>% 
  mutate(nowork=sum(item2566, item2567, na.rm=TRUE))

sum(checkoccsprfprev$occsp, na.rm=TRUE)
sum(checkoccsprfprev$nowork, na.rm=TRUE)


checkoccspreal<-jac_disag %>% 
  filter(survtype=="Non-SAF") %>% 
  select(parish, holding, ags_madeup, survtype, item177, item178, item179, item182, item183, item184, item2566, item2567) %>% 
  rowwise() %>% 
  mutate(occsp=sum(item177, item178, item179, item182, item183, item184, na.rm=TRUE)) %>% 
  mutate(nowork=sum(item2566, item2567, na.rm=TRUE))

sum(checkoccspreal$occsp, na.rm=TRUE)
sum(checkoccspreal$nowork, na.rm=TRUE)


checkoccsprf<-jac_disag %>% 
  filter(survtype!="Non-SAF") %>% 
  select(parish, holding, ags_madeup, survtype, item177, item178, item179, item182, item183, item184, item2566, item2567) %>% 
  rowwise() %>% 
  mutate(occsp=sum(item177, item178, item179, item182, item183, item184, na.rm=TRUE)) %>% 
  mutate(nowork=sum(item2566, item2567, na.rm=TRUE))

sum(checkoccsprf$occsp, na.rm=TRUE)
sum(checkoccsprf$nowork, na.rm=TRUE)



# check migrant labour


migrants<-jac_totals %>% 
  select(parish, holding, survtype, land_data, imptype, item2511, item2536)

item50final<-jac_totals %>% 
  select(parish, holding, survtype, land_data, imptype, item46, item50)

item50<-whole_population %>% 
  select(parish, holding, survtype, land_data, imptype, item46, item47, item48, item49, item50)


holdingsitem50<-df_nonSAF %>% 
  filter((parish==94&holding==49)|(parish==662&holding==44)|(parish==673&holding==10)|(parish==669&holding==4)|(parish==674&holding==44))


# check workforce

workers<-jac_totals %>% 
  filter(imptype=="none") %>% 
select(parish, holding,imptype,submis_type, "item1714", "item1715", "item1716", "item1717", "item192", "item193", "item1718", "item194", "item195", "item1719", "item196", "item197", "item198", "item199", "item200")


sum(workers$item1717, na.rm=TRUE)

checkturkeys<-jac_totals %>% 
  select(parish, holding, imptype, item1708, item2038, item2039, item167)


checkturkeysprev<-previous_years %>% 
  filter(yr==2021 &ags_madeup==0) %>% 
  select(parish, holding, imptype, item1708, item2038, item2039, item167)

sum(checkturkeysprev$item1708, na.rm=TRUE)

checkitem40<-jac_disag %>% 
  select(parish, holding, item40,item14, item15, item16, item18, item17, item20, item3156, item19, 
         item23, item21, item24, item2320, item27, item28, item2034, item29, item30,
         item31, item2059, item32, item34, item36, item2469, item2470, item35, item37, item38)

tables<- tables_nice %>% pivot_longer(everything(), names_to = "categories", values_to = "number")



write.csv(tables,"//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Main/June23/tables_latest_11.10.23.csv" )

# Save data

write_dataframe_to_db(
  server = server,
  database = database,
  schema = schema,
  table_name = "JAC23_final_dataset_totals",
  dataframe = jac_totals,
  append_to_existing = FALSE,
  versioned_table = FALSE,
  batch_size = 10000
)  





