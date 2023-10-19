
# Before import -----------------------------------------------------------


# Clear environment prior

rm(list = ls())

source("item_numbers.R")


# Load packages

library(readxl)
library(tidyverse)
library(stringr)
library(janitor)
library(data.table)
library(skimr)
#library(haven)
library(RtoSQLServer)


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

#Import-------------------------------------------------------------



jac <- read_table_from_db(server=server, 
                          database=database, 
                          schema=schema, 
                          table_name="JAC23_temp")


jac$id<-paste0(jac$parish,"_",jac$holding)

jac$yr<-2023

names(jac) <- gsub("CTS", "cts", names(jac))

# Load previous years

# previous_years <- read_table_from_db(server=server,
#                                      database=database,
#                                      schema=schema,
#                                      table_name="jac_previous_data_ten_years")


load(paste0(Code_directory, "/previous_years.rda"))



# Correction just for this run - 10.10.23 - delete on any subsequent run!



check<-jac %>% 
  select(id, imptype, item27750, item27755, item85, item86, item38, item40, land_data)



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
  )


land_final <- land_split %>% 
  select(id, land_data, imptype, madeup, saf_madeup, ags_madeup, land_items)

check2<-land_final %>% 
  select(id, item27710, item16, item18, item27730,item52,item53,item55,item56,item2323,item59,item60,item61,item63,item64,item65,item66)


checkholdingspost<-land_final %>% 
  select(id, item27710, item16, item18) %>% 
  filter(id=="666_78"|id=="816_58" |id=="417_8")

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
                item155=prop_155*item27760)

livestock_final<-livestock_final %>% 
  select(id, livestock_items)

# Overwrite final dataset

jac$id<-paste0(jac$parish,"_",jac$holding)


#Jackie - convert logical NA to double otherwise below code won't work 
# jac$item2713 <- as.double(jac$item2713)
# jac$item2714 <- as.double(jac$item2714)
# jac$item2715 <- as.double(jac$item2715)
# jac$item2716 <- as.double(jac$item2716)
# jac$item2717 <- as.double(jac$item2717)
# jac$item2866 <- as.double(jac$item2866)
# jac$item2708 <- as.double(jac$item2708)
# jac$item2709 <- as.double(jac$item2709)
# jac$item2710 <- as.double(jac$item2710)
# jac$item2711 <- as.double(jac$item2711)
# jac$item152 <- as.double(jac$item152)
# jac$item153 <- as.double(jac$item153)
# jac$item154 <- as.double(jac$item154)
# jac$item155 <- as.double(jac$item155)
# jac$item95<- as.double(jac$item95)
# jac$item96<- as.double(jac$item96)
# jac$item1712<- as.double(jac$item1712)
# jac$item1713<- as.double(jac$item1713)
#jac$item98 <- as.double(jac$item98)

sapply(jac, class)

jac <- jac %>% dplyr::mutate(across(where(is.logical), ~ as.double(.x)))

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


jac_disagorig<-jac_disag


# Corrections to data


jac_disag<-ungroup(jac_disag)



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


jac_disag<-jac_disag %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(
    item38 = sum(item41,item84,item85,item86, na.rm = TRUE),
    item40 = sum(item14, item15, item16, item18, item17, item20, item3156, item19, 
                 item23, item21, item24, item2320, item27, item28, item2034, item29, item30,
                 item31, item2059, item32, item34, item36, item2469, item2470, item35, item37, item38, na.rm=TRUE),
    item46 = sum(item2321, item2322, item40, na.rm=TRUE),
    item50 = sum(item46, item47, item48, item49, na.rm=TRUE))



# Disaggregate remaining holdings by overall proportion

# Pigs

pigs <- jac_disag %>% 
  select(id, item27760, item152, item153, item154, item155) %>% 
  dplyr::mutate(totalitem152=sum(item152, na.rm=TRUE),
         totalitem153=sum(item153, na.rm=TRUE),
         totalitem154=sum(item154, na.rm=TRUE),
         totalitem155=sum(item155, na.rm=TRUE),
         totalitem27760=sum(item27760, na.rm=TRUE),
         prop_152=totalitem152/(totalitem152+totalitem153+totalitem154+totalitem155),
         prop_153=totalitem153/(totalitem152+totalitem153+totalitem154+totalitem155),
         prop_154=totalitem154/(totalitem152+totalitem153+totalitem154+totalitem155),
         prop_155=totalitem155/(totalitem152+totalitem153+totalitem154+totalitem155)) %>% 
  mutate(item152= ifelse (is.na(item152), prop_152*item27760, item152),
         item153= ifelse (is.na(item153), prop_153*item27760, item153),
         item154= ifelse (is.na(item154), prop_154*item27760, item154),
         item155= ifelse (is.na(item155), prop_155*item27760, item155)) %>% 
  select(id, item27760, item152, item153, item154, item155)


jac_disag<-rows_update(jac_disag, pigs, by="id")

jac_disag<-jac_disag %>% 
  dplyr::mutate(across(any_of(c(all_pig)), round_half_up, 0))


checkitem50<-jac_disag %>% 
  select(id, item50, item46,item12, item40,item40,item14, item15, item16, item18, item17, item20, item3156, item19, 
         item23, item21, item24, item2320, item27, item28, item2034, item29, item30,
         item31, item2059, item32, item34, item36, item2469, item2470, item35, item37, item38)


# Save temp dataset -------------------------------------------------------

write_dataframe_to_db(
  server = server,
  database = database,
  schema = schema,
  table_name = "JAC23_temp",
  dataframe = jac_disag,
  append_to_existing = FALSE,
  versioned_table = FALSE,
  batch_size = 10000
)

