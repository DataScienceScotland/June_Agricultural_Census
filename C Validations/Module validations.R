#JAC Validations for Census form data (C2, C3, C4 IN SAS)


library(tidyverse)
library(RtoSQLServer)
library(writexl)
library(skimr)

# Clear environment prior
rm(list = ls())

#yr = this year 20xx
yr <- 2023
yr1 <- yr-1 
yr2 <-  yr-2
yr3 <-  yr-3
yr_list <- c(yr, yr2, yr3)




# Before import -----------------------------------------------------------




# Datashare file path for import and export
output_path <- "//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Codeconversion_2023/"
sas_agscens_path <- "//s0177a/sasdata1/ags/census/agscens/"


# ADM schema for export
server <- "s0196a\\ADM"
database <- "RuralAndEnvironmentalScienceFarmingStatistics"
schema <- "juneagriculturalsurvey2023alpha"


#Core item list

#Section 1 - Area of location----------------------------------------------------------------------------------------------------------------------------
#Total area
total_area <- "item12"

#Module item list----------------------------------------------------------------------------------------------------------------------------------------------------


#Section 12 – Agricultural production methods---------------------------------------------------------------

rough_grazing_woodland_only <- "item20705"

#Crop_rotation and cropping land
crop_rotation <- "item2653"
not_crop_rotation <- "item2654"
arable_mix_with_legume <- "item2680"

#Soil cover on cropping land
autumn_winter_crops <- "item2358"
field_veg_potatoes	<- "item2359"
multi_annual	<- "item3027"
cover_legume	<- "item2360"
cover_non_legume	<- "item2365"
plant_residues_stubble	<- "item2361"
bare_soil	<- "item2362"

#Tillage
zero_tillage	<- "item2655"
reduced_tillage	<- "item2364"
inversion_tillage	<- "item2369"

#Irrigation
irrrigation	<- "item2638"

#Nutrient management
soil_testing_grass	<- "item3028"
soil_testing_crop	<- "item3029"
soil_testing_change_nutrient	<- "item3032"
area_ph_grass	<- "item3512"
area_ph_crop	<- "item3513"
nutrient_plan_update	<- "item3514"
nutrient_plan_grass	<- "item2656"
nutrient_plan_crop	<- "item2657"
grass_legume_sward	<- "item2681"
nitrogen_mixed_sward	<- "item2682"
manure_mixed_sward	<- "item2683"
protected_urea	<- "item2684"
precision_farming	<- "item2685"

section_12 <- c(rough_grazing_woodland_only,crop_rotation, not_crop_rotation, arable_mix_with_legume,
                autumn_winter_crops, field_veg_potatoes, multi_annual, cover_legume, cover_non_legume,
                plant_residues_stubble, bare_soil, zero_tillage, reduced_tillage, inversion_tillage,
                irrrigation, soil_testing_grass, soil_testing_crop, soil_testing_change_nutrient, area_ph_grass,
                area_ph_crop, nutrient_plan_update, nutrient_plan_grass, nutrient_plan_grass, nutrient_plan_crop,
                grass_legume_sward, nitrogen_mixed_sward, manure_mixed_sward, protected_urea, precision_farming)

#Section 13 – Manure, slurry and mineral fertiliser application-------- ------------------------------------------------

ghouse_fallow_woodland_oland_only <- "item21207"

#Area fertilised
fertiliser_mineral_area	<- "item5100"
fertiliser_organic_area	<- "item5101"

#Solid manure use and import/export
manure_use	<- "item5129"
solid_manure_export	<- "item5104"
solid_manure_import <- "item5105"

#Slurry/liquid manure use and import/export
slurry_used	<- "item5102"
slurry_export	<- "item5106"
slurry_import	<- "item5107"

#Mineral fertiliser and other organic fertilisers
mineral_fertilisers_spread	<- "item5130"
organic_fertilisers_spread	<- "item5103"


#What percentage of the manure/slurry spread on your holding is spread using the following techniques? 
broadcast_spreader_less_four_hours <- "item5108"
broadcast_spreader_more_four_hours	<- "item5109"
broadcast_spreader_not_ploughed	<- "item5110"
band_spreader_hose	<- "item5111"
band_spreader_shoe	<- "item5112"
open_slot_shallow_injection	<- "item5113"
closed_slot_deep_injection	<- "item5114"
other_spread	<- "item5131"

#Please indicate the percentage of total manure or slurry stored (including that eventually exported) in the following systems
storage_heaps	<- "item5115"
storage_compost	<- "item5116"
storage_pits_below_animals	<- "item5117"
storage_deep_litter	<- "item5118"
storage_without_cover	<- "item5119"
storage_permeable_cover	<- "item5120"
storage_impermeable_cover	<- "item5121"
storage_other_facilities	<- "item5122"
daily_spread	<- "item5123"
other_systems	<- "item5132"

#Please enter the number of months for which manure produced on the holding can be held, without risk of runoff, and without occasional emptying.
manure_compost_months	<- "item5124"
manure_pits_months	<- "item5125"
manure_deep_litter_months	<- "item5126"
slurry_months	<- "item5127"
manure_other_facilities_months	<- "item5128"

#Manure (over the last 12 months)
tested_nutrient_value	<- "item2662"
separate_applications	<- "item2663"

section_13 <- c(ghouse_fallow_woodland_oland_only, fertiliser_mineral_area, fertiliser_organic_area, manure_use, solid_manure_export,
                solid_manure_import, slurry_used, slurry_export, slurry_import, mineral_fertilisers_spread, organic_fertilisers_spread,
                broadcast_spreader_less_four_hours, broadcast_spreader_more_four_hours, broadcast_spreader_not_ploughed, band_spreader_hose, band_spreader_shoe,
                open_slot_shallow_injection, closed_slot_deep_injection, other_spread, storage_heaps, storage_compost,
                storage_pits_below_animals, storage_deep_litter, storage_without_cover, storage_permeable_cover, storage_impermeable_cover,
                storage_other_facilities, daily_spread, other_systems, manure_compost_months, manure_pits_months, manure_deep_litter_months,
                slurry_months, manure_other_facilities_months, tested_nutrient_value, separate_applications)


#Module error descriptions ------------------------------------------

merr1_desc <- "merr1_Total percentage of the manure/slurry spread not equal to 100%"
merr2_desc <- "merr2_Total percentage of the manure/slurry storage not equal to 100%"
merr3_desc <- "merr3_Indicated nutrient management but no area of grassland or cropland given"
merr4_desc <- "merr4_Sum of general crop rotation land and land not included in general crop rotation differs from total area (item12) by 3 ha or more"
merr5_desc <- "merr5_Sum of area pH tested (grassland and cropland) is greater than total area (item12)"
merr6_desc <- "merr6_Sum of area nutrient management plan (grassland and cropland) is greater than total area (item12)"
merr7_desc <- "merr7_Sum of total farmed area fertilised with mineral and manure fertilisers greater than total area (item12)"


# Import ------------------------------------------------------------------

#combined dataset (SAF and JAC data)


combined_JAC <- read_table_from_db(server=server, 
                                   database=database, 
                                   schema=schema, 
                                   table_name="combined_data_2023")


combined_JAC <- combined_JAC %>% group_by(parish, holding)

#filter for only form returns
all_JAC_form <- combined_JAC %>% filter(survtype == "Non-SAF") 

#recode-------------------------------------------------------------------
#for info
#0 = Missing answer (NAs converted to 0 in B2 )
#1= Yes
#2 = No
#3 = NA (answered as NA on form)

#data explorer---------------------
str(JAC_module_23)
#data classes
sapply(JAC_module_23, function(x)class(x))
#range of values 
sapply(JAC_module_23, function(x)range(x))
#summary
JAC_module_23 %>% skim()


#section counts--------------------------------------------------------------------------------------------------------
#write code for NAs from paper....
no_section_12 <- all_JAC_form %>% select (all_of(section_12)) %>%  filter(get(rough_grazing_woodland_only)== 1)
yes_section_12 <- all_JAC_form %>% select (all_of(section_12)) %>%  filter(get(rough_grazing_woodland_only)== 0)



no_section_13 <- all_JAC_form %>% select (all_of(section_13)) %>%  filter(get(ghouse_fallow_woodland_oland_only)== 1) 
yes_section_13 <- all_JAC_form %>% select (all_of(section_13)) %>%  filter(get(ghouse_fallow_woodland_oland_only)== 0)


no_section_12_13 <- all_JAC_form %>% select (all_of(c(section_12, section_13)))%>% 
  filter(get(rough_grazing_woodland_only) ==1 & get(ghouse_fallow_woodland_oland_only) ==1)
yes_section_12_13 <- all_JAC_form %>% select (all_of(c(section_12, section_13)))%>% 
  filter(get(rough_grazing_woodland_only) ==0 & get(ghouse_fallow_woodland_oland_only) ==0)

yes_no_section_12_13 <- all_JAC_form %>% select (all_of(c(section_12, section_13)))%>% 
  filter(get(rough_grazing_woodland_only) ==0 & get(ghouse_fallow_woodland_oland_only) ==1 |
           get(rough_grazing_woodland_only) ==1 & get(ghouse_fallow_woodland_oland_only) ==0)
           


count_no_section_12 <- nrow(no_section_12)
count_yes_section_12 <- nrow(yes_section_12)
count_section_12 <- sum(count_no_section_12, count_yes_section_12)

count_no_section_13 <- nrow(no_section_13)
count_yes_section_13 <- nrow(yes_section_13)
count_section_13 <- sum(count_no_section_13, count_yes_section_13)


count_no_section_12_13 <- nrow(no_section_12_13)
count_yes_section_12_13 <- nrow(yes_section_12_13)
count_yes_no_section_12_13 <- nrow(yes_no_section_12_13)
count_section_12_13 <- sum(count_no_section_12_13, count_yes_section_12_13, count_yes_no_section_12_13)
#count_section_12_13_na <- all_JAC_form %>% filter(is.na(get(rough_grazing_woodland_only)) | is.na(get(ghouse_fallow_woodland_oland_only)))


module_section_counts <- data.frame(count_no_section_12, count_yes_section_12, count_no_section_13, count_yes_section_13, count_no_section_12_13, 
                                    count_yes_section_12_13, count_yes_no_section_12_13, count_section_12, count_section_13, count_section_12_13) %>%
  pivot_longer(cols= everything(), names_to = "category", values_to = "count") %>%
  mutate(., percentage = (count/count_section_12_13)*100)


#percentage checks-------------------------------------------------------------------------------------------------

JAC_module_23<- all_JAC_form %>% select(parish, holding, submisType, 
                                        land_data, saf_data, all_of(c(section_12, section_13, total_area)))

#check number of 0 responses across module
no_response <- JAC_module_23 %>% ungroup %>% select(-parish, -holding, -land_data, -saf_data, ) %>% mutate(no_response = rowSums(.)) %>% filter(no_response == 0) %>% 
  nrow(.)

#manure/slurry spread percentage
JAC_module_23 <- JAC_module_23 %>%  mutate(manure_slurry_spread_total = sum(get(broadcast_spreader_less_four_hours), get(broadcast_spreader_more_four_hours), get(broadcast_spreader_not_ploughed),
                                                                     get(band_spreader_hose), get(band_spreader_shoe), get(open_slot_shallow_injection), 
                                                                     get(closed_slot_deep_injection), get(other_spread)))

#merr1 - manure/slurry spread total doesn't add to 100%
JAC_module_23 <- JAC_module_23 %>% mutate(merr1 = case_when(manure_slurry_spread_total == 100 ~ 0,
                                                            manure_slurry_spread_total == 0 ~ 0,
                                                            TRUE ~1))


##manure/slurry stored total percentage
JAC_module_23 <- JAC_module_23 %>%  mutate(manure_slurry_stored_total = sum(get(storage_heaps), get(storage_compost),
                                                                     get(storage_pits_below_animals), get(storage_deep_litter), get(storage_without_cover), 
                                                                     get(storage_permeable_cover), get(storage_impermeable_cover), get(storage_other_facilities), 
                                                                     get(daily_spread), get(other_systems)))


#merr2 - manure/slurry stored total doesnt't add to 100%
JAC_module_23 <- JAC_module_23 %>% mutate(merr2 = case_when(manure_slurry_stored_total == 100 ~ 0,
                                                            manure_slurry_stored_total == 0 ~ 0,
                                                            TRUE ~1))

#merr3 - indicated nutrient management but no area of grassland or cropland given

JAC_module_23 <- JAC_module_23 %>% mutate(merr3 = case_when(nutrient_plan_update ==1 & (get(nutrient_plan_crop) == 0 |
                                                                                          get(nutrient_plan_grass) ==0) ~ 1,
                                                            TRUE ~0))

#merr4 - sum of general crop rotation land and land not included in general crop rotation differs from total area (item12) by 3 ha or more
#check in 'module error dataframes' section to see if land has been erroneously portioned between general crop rotation, not in general crop rotation and arable mix with legume
#should be general crop rotation/not in general crop rotation. 

#create crop_rot_sum (general crop rot + not in general crop rot) 
JAC_module_23 <- JAC_module_23 %>% mutate(crop_rot_sum = sum(get(crop_rotation), get(not_crop_rotation)))

#merr4 = 1 when difference between crop_rot_sum and total area > 3 
JAC_module_23 <- JAC_module_23 %>% mutate(merr4 = case_when(crop_rot_sum > 0 & abs(round(crop_rot_sum, digits = 2) - round(get(total_area), digits = 2)) >= 3 ~ 1,
                                                            TRUE~0))
#add column to give difference between crop_rot_sum and total area 
JAC_module_23 <- JAC_module_23 %>% mutate(merr4_diff = case_when(merr4 == 1 ~ abs(crop_rot_sum - get(total_area)),
                                                            TRUE~0))

#merr5 - sum of area pH tested (grassland and cropland) is greater than total area (item12)

#create sum area pH grassland and cropland
JAC_module_23 <- JAC_module_23 %>% mutate(pH_sum = sum(get(area_ph_crop), get(area_ph_grass)))

JAC_module_23 <- JAC_module_23 %>% mutate(merr5 = case_when(round(pH_sum, digits = 2) > round(get(total_area), digits = 2) ~ 1,
                                                            TRUE ~0))

JAC_module_23 <- JAC_module_23 %>% mutate(merr5_diff = case_when(merr5==1~abs(pH_sum-get(total_area)),
                                                            TRUE ~0))


#merr6 - sum of area of nutrient management plan (grassland and cropland) is greater than total area (item12)

#create sum nutrient management grassland and cropland
JAC_module_23 <- JAC_module_23 %>% mutate(nutrient_sum = sum(get(nutrient_plan_grass), get(nutrient_plan_crop)))

JAC_module_23 <- JAC_module_23 %>% mutate(merr6 = case_when(round(nutrient_sum, digits = 2) > round(get(total_area), digits = 2) ~ 1,
                                                            TRUE ~0))

JAC_module_23 <- JAC_module_23 %>% mutate(merr6_diff = case_when(merr6==1~abs(nutrient_sum-get(total_area)),
                                                                 TRUE ~0))

#merr7- sum of total farmed area fertilised with mineral and manure fertilisers greater than total area (item12)

#create sum for area fertilised with mineral and manure fertilisers 
JAC_module_23 <- JAC_module_23 %>% mutate(fertiliser_sum = sum(get(fertiliser_mineral_area), get(fertiliser_organic_area)))

JAC_module_23 <- JAC_module_23 %>% mutate(merr7 = case_when(round(fertiliser_sum, digits = 2) > round(get(total_area), digits = 2) ~ 1,
                                                            TRUE ~0))

JAC_module_23 <- JAC_module_23 %>% mutate(merr7_diff = case_when(merr7==1~abs(fertiliser_sum-get(total_area)),
                                                                 TRUE ~0))



#module error dataframes------------------------
#merr1-----------------------------
##@knitr merr1
merr1<- JAC_module_23%>% select(parish, holding, submisType, 
                                land_data, saf_data, all_of(c(broadcast_spreader_less_four_hours, broadcast_spreader_more_four_hours, broadcast_spreader_not_ploughed,
                                                                                                       band_spreader_hose, band_spreader_shoe,open_slot_shallow_injection, 
                                                                                                       closed_slot_deep_injection, other_spread)), manure_slurry_spread_total, merr1)
merr1 <-merr1%>%  mutate(error=ifelse(merr1== 1 | is.na(merr1), "Error", "No Error"))%>% filter(error == "Error")

#merr2-----------------------------
##@knitr merr2
merr2<- JAC_module_23%>% select(parish, holding, submisType, 
                                land_data, saf_data, all_of(c(storage_heaps, storage_compost,
                                                              storage_pits_below_animals, storage_deep_litter, storage_without_cover, 
                                                              storage_permeable_cover, storage_impermeable_cover, storage_other_facilities, 
                                                              daily_spread, other_systems)), manure_slurry_stored_total, merr2)
merr2 <-merr2%>%  mutate(error=ifelse(merr2== 1 | is.na(merr2), "Error", "No Error"))%>% filter(error == "Error")

#merr3-----------------------------
##@knitr merr3
merr3<- JAC_module_23%>% select(parish, holding, submisType, 
                                land_data, saf_data, all_of(c(nutrient_plan_update, nutrient_plan_crop, nutrient_plan_grass)), merr3)
merr3 <-merr3%>%  mutate(error=ifelse(merr3== 1 | is.na(merr3), "Error", "No Error"))%>% filter(error == "Error")


#merr4-----------------------------
##@knitr merr4
merr4<- JAC_module_23%>% select(parish, holding, submisType, 
                                land_data, saf_data, all_of(c(total_area, crop_rotation, not_crop_rotation, arable_mix_with_legume)), crop_rot_sum, merr4, merr4_diff)
merr4 <-merr4%>%  mutate(error=ifelse(merr4== 1 | is.na(merr4), "Error", "No Error"))%>% filter(error == "Error")

#check if people have portioned land between general crop rotation, not in general crop rotation and arable mix with legume
merr4_check <- merr4 %>% filter(merr4, abs(crop_rot_sum - get(total_area)) == get(arable_mix_with_legume))

#merr5-----------------------------
##@knitr merr5
merr5<- JAC_module_23%>% select(parish, holding, submisType, 
                                land_data, saf_data, all_of(c(total_area, area_ph_crop, area_ph_grass)), pH_sum, merr5, merr5_diff)
#check filter(merr5, pH_sum>get(total_area))
merr5 <-merr5%>%  mutate(error=ifelse(merr5== 1 | is.na(merr5), "Error", "No Error"))%>% filter(error == "Error")

#merr6-----------------------------
##@knitr merr6
merr6<- JAC_module_23%>% select(parish, holding, submisType, 
                                land_data, saf_data, all_of(c(total_area, nutrient_plan_crop, nutrient_plan_grass)), nutrient_sum, merr6, merr6_diff)

# check filter(merr6, nutrient_sum>get(total_area)) 
merr6 <-merr6%>%  mutate(error=ifelse(merr6== 1 | is.na(merr6), "Error", "No Error"))%>% filter(error == "Error")

#merr7-----------------------------
##@knitr merr7
merr7<- JAC_module_23%>% select(parish, holding, submisType, 
                                land_data, saf_data, all_of(c(total_area, fertiliser_mineral_area, fertiliser_organic_area)), fertiliser_sum, merr7, merr7_diff)

# check filter(merr7, fertiliser_sum>get(total_area)) 
merr7 <-merr7%>%  mutate(error=ifelse(merr7== 1 | is.na(merr7), "Error", "No Error"))%>% filter(error == "Error")










##module error summary----------------------
merr_list <- c("merr1", "merr2", "merr3")
module_errors <- JAC_module_23 %>% select(parish, holding, starts_with("merr"))%>% 
  filter(get(merr_list)!=0)

JAC_module_23 %>% select(arable_mix_with_legume)
  
                                             