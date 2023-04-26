#JAC Validations for Census form data (C2, C3 IN SAS)
#NB: when changing for 2023, area_rent_21 (item7) == area_rent (item20026)  




library(tidyverse)
#yr = this year 20xx
yr <- 21
yr1 <- yr-1 
yr2 <-  yr-2
yr3 <-  yr-3
yr_list <- c(yr, yr1, yr2)




# Before import -----------------------------------------------------------


# Clear environment prior
rm= ls()

# Datashare file path for import and export
output_path <- "//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Codeconversion_2023/"
sas_agscens_path <- "//s0177a/sasdata1/ags/census/agscens/"


# ADM schema for export
server <- "s0196a\\ADM"
database <- "RuralAndEnvironmentalScienceFarmingStatistics"
schema <- "juneagriculturalsurvey2023alpha"



# Import ------------------------------------------------------------------
#item_lists
source("item_numbers.R")
#combined dataset (SAF and JAC data)
SAS_combined <- read.csv(paste0(output_path, "COMBINED_DATA.csv"))
SAS_combined <- SAS_combined %>% group_by(parish, holding)

#croft dataset
load(paste0(output_path, "crofts_A.rda"))
croft <- df_croftsfinal %>% group_by(parish, holding)



# Combine croft and combined dataset------------------------------------------------------------------

#filter for only form returns
JAC_form <- SAS_combined %>% filter(survtype == "Non-SAF"| survtype == "SAF") 
croft <- croft %>% mutate(croft = 1)

#form returns and croft dataset
all_JAC_form<- left_join(JAC_form, croft, by = c("parish", "holding")) %>% 
  mutate(croft = case_when(croft ==1 ~ as.numeric(croft),
                           TRUE ~ 0))

#Validations---------------------------------------------------------------------------------------

#NOTE: a value of 1 in the "err(x)" column = holding data has failed logic of validation test. A value of 0 means data for that holding are fine.

# Section 1 Area of holding--------------------
#err1 = err2 in SAS
#Total area = area rent + area own
all_JAC_form$err1 <- ifelse(round(all_JAC_form[total_area], digits = 2) != round(all_JAC_form[area_own]+all_JAC_form[area_rent_21], digits = 2),  1, 0)



# Section 2 Seasonal rents--------------------

#err2 = err10 in SAS
#Total seasonal rent area = area rent in section 1
all_JAC_form$err2 <- ifelse(round(all_JAC_form[seas_total_area_in], digits = 2) == round(all_JAC_form[area_rent_21], digits = 2)
                            & (all_JAC_form[seas_total_area_in] & all_JAC_form[area_rent_21]> 0) & all_JAC_form$land_data == "ags",  1, 0)
#err3 = err11 in SAS 
#new seasonal rent + same location seasonal rent = total seasonal rent area
all_JAC_form$err3 <- ifelse(round(all_JAC_form[seas_rent_diff_in] + all_JAC_form[seas_rent_same_in], digits = 2) != round(all_JAC_form[seas_total_area_in], digits = 2)
                            & all_JAC_form$land_data != "both",  1, 0)


# Section 3 Land Use --------------------------

#Validate/QA SAF totals (adding itemised crops) vs form summary totals?

#name of other crop given if area > 100 ha
#err4 = err13 in SAS
all_JAC_form$err4 <-  ifelse(all_JAC_form[other_crops] > 100 & is.na(all_JAC_form[other_crops_text]),  1, 0)


#grassland-------------------------------------------------------------------------------------------------------------------------------------------------
#err5 = err15 in SAS
total7 <- c(total_crops_grass, rough_graze, woodland, other_land)
all_JAC_form$total7 <- rowSums(all_JAC_form[total7], na.rm =TRUE)

all_JAC_form$err5 <-  
  ifelse(all_JAC_form$land_data == "ags", ifelse(round(all_JAC_form[total_land], digits = 2) != round(all_JAC_form$total7, digits = 2), 1, 0), 0)

##NEED TO FIX: 1065 entries vs SAS 947
##Noted e.g. P/H: 2/27: item50 = 0 in R but sas (agstemp.validations21) item50 = 2.5?  

# all_JAC_form %>% filter(err5 ==1 & land_data == "ags") %>% select(contains(c(total7, total_land)), err5, total7)

# 
# all_JAC_form %>% filter(parish == 2, holding == 27) %>%  select(contains(c(total7, total_land)), err5)
#----------------------------------------------------------------------------------------------------------------------------------------

#Vegetables
total2 <- c(peas, beans,leeks, turn_swed, cab_sav, brussel, calabrese, cauli_broc, carrots,lettuce, rhubarb, other_veg) 
all_JAC_form$total2 <- rowSums(all_JAC_form[total2], na.rm =TRUE)
#err6 = err18 in SAS
#all_JAC_form$err6 <-  ifelse(round(all_JAC_form[all_veg_saf], digits = 2) != round(all_JAC_form$total2, digits = 2), 1, 0)
#err7 = err19 in SAS
all_JAC_form$err7 <-  ifelse(round(all_JAC_form[all_veg_saf], digits = 2) != round(all_JAC_form[all_veg_open_21], digits = 2), 1, 0)

#Soft fruit
total3 <- c(strawberries, raspberries, blackcurrants, blueberries, mixed_soft_fruit)
all_JAC_form$total3 <- rowSums(all_JAC_form[total3], na.rm =TRUE)
#err8 = err20 in SAS
#all_JAC_form$err8 <-  ifelse(round(all_JAC_form[all_fruit_saf], digits = 2) != round(all_JAC_form$total3, digits = 2), 1, 0)
#err9 = err21 in SAS
all_JAC_form$err9 <-  ifelse(round(all_JAC_form[soft_fruits], digits = 2) != round(all_JAC_form[all_fruit_saf], digits = 2), 1, 0)

#Flowers, bulbs and nursery stock grown in the open
total4 <- c(all_flow_bulb_SAF, bed_pot_plant, fruit_stocks, roses_stocks, orn_trees, shrubs, other_nurs)
all_JAC_form$total4 <- rowSums(all_JAC_form[total4], na.rm =TRUE)
#err10 = err22 in SAS
#all_JAC_form$err10 <-  ifelse(round(all_JAC_form[all_flow_bulb_21], digits = 2) != round(all_JAC_form$total4, digits = 2), 1, 0)
#err11 = err23 in SAS
total5 <- c(glass_tom_open, glass_other_fruit_open, glass_veg_open, glass_bed_pot_plant_open, 
            glass_nurs_stock_open, glass_strawb_open, glass_rasp_open, glass_blue_open, glass_black_open, glass_unused_open)
total6 <- c(glass_tom_solid, glass_other_fruit_solid, glass_veg_solid, glass_bed_pot_plant_solid,
            glass_nurs_stock_solid, glass_strawb_solid, glass_rasp_solid, glass_blue_solid, glass_black_solid, glass_unused_solid)
all_JAC_form$total5 <- rowSums(all_JAC_form[total5], na.rm =TRUE)
all_JAC_form$total6 <- rowSums(all_JAC_form[total6], na.rm =TRUE)
all_JAC_form$err11<-  
  ifelse(all_JAC_form$survtype == "Non-SAF", ifelse(round(all_JAC_form$total5, digits = 2) != round(all_JAC_form[tot_open_plastic], digits = 2)| 
                                                      round(all_JAC_form$total6, digits = 2) != round(all_JAC_form[tot_solid_glass], digits = 2),  1, 0), 0)
#23 obs vs 25 in SAS



#err12 = err24 in SAS
all_JAC_form$err12 <- ifelse(all_JAC_form[tot_open_plastic]+all_JAC_form[tot_solid_glass]>0 & (all_JAC_form$total5 + all_JAC_form$total6)<=0, 1, 0 )


# Section 4 Pigs--------------------

#2023 version
#total10 <- c(sows_pig, gilts_pig, other_sows_pig, barren_sows_pig, gilts_50_pig, boars_pig, fat_pig, wean_pig, piglet)
total10_21 <- c(sows_pig, gilts_pig, other_sows_pig, barren_sows_pig, gilts_50_pig, boars_pig, other_110_pig, other_80_110_pig, other_50_80_pig, other_20_50_pig, other_lt20_pig)

all_JAC_form$total10 <-rowSums(all_JAC_form[total10_21], na.rm =TRUE)
#err13 = err25 in SAS
all_JAC_form$err13 <-  ifelse(round(all_JAC_form$total10, digits = 1) != round(all_JAC_form[total_pig], digits = 1), 1, 0)
#err14 = err26 in SAS
all_JAC_form$err14 <-  ifelse(round(all_JAC_form[sows_pig], digits = 1) != all_JAC_form[sows_pig]|
                                round(all_JAC_form[gilts_pig], digits = 1) != all_JAC_form[gilts_pig]|
                              round(all_JAC_form[other_sows_pig], digits = 1) != all_JAC_form[other_sows_pig]|
                                round(all_JAC_form[barren_sows_pig], digits = 1) != all_JAC_form[barren_sows_pig]|
                                round(all_JAC_form[gilts_50_pig], digits = 1) != all_JAC_form[gilts_50_pig] |
                                round(all_JAC_form[boars_pig], digits = 1) != all_JAC_form[boars_pig]|
                                round(all_JAC_form[other_110_pig], digits = 1) != all_JAC_form[other_110_pig]|
                                round(all_JAC_form[other_80_110_pig], digits = 1) != all_JAC_form[other_80_110_pig]|
                                round(all_JAC_form[other_50_80_pig], digits = 1) != all_JAC_form[other_50_80_pig]|
                                round(all_JAC_form[other_20_50_pig], digits = 1) != all_JAC_form[other_20_50_pig]|
                                round(all_JAC_form[other_lt20_pig], digits = 1) != all_JAC_form[other_lt20_pig], 1, 0)

#new pig item numbers for 2023 - comment out above and uncomment below
#all_JAC_form$err14 <-  ifelse(round(all_JAC_form[sows_pig], digits = 1) != all_JAC_form[sows_pig]|
                                # round(all_JAC_form[gilts_pig], digits = 1) != all_JAC_form[gilts_pig]|
                                # round(all_JAC_form[other_sows_pig], digits = 1) != all_JAC_form[other_sows_pig]|
                                # round(all_JAC_form[barren_sows_pig], digits = 1) != all_JAC_form[barren_sows_pig]|
                                # round(all_JAC_form[gilts_50_pig], digits = 1) != all_JAC_form[gilts_50_pig] |
                                # round(all_JAC_form[boars_pig], digits = 1) != all_JAC_form[boars_pig]|
                                # round(all_JAC_form[fat_pig], digits = 1) != all_JAC_form[fat_pig]|
                                # round(all_JAC_form[wean_pig], digits = 1) != all_JAC_form[wean_pig]|
                                # round(all_JAC_form[piglet], digits = 1) != all_JAC_form[piglet], 1, 0)

#Section 5 Sheep--------------------

total9 <- c(ewes, rams, breed_other_sheep, no_breed_other_sheep, lambs)
all_JAC_form$total9 <- rowSums(all_JAC_form[total9], na.rm =TRUE)
#err15 = err30 in SAS
all_JAC_form$err15 <-  ifelse(round(all_JAC_form$total9, digits = 1) != round(all_JAC_form[total_sheep], digits = 1), 1, 0)
#err16 = err31 in SAS
all_JAC_form$err16 <-  ifelse(round(all_JAC_form[ewes], digits = 1) != all_JAC_form[ewes]|
                                round(all_JAC_form[rams], digits = 1) != all_JAC_form[rams]|
                                round(all_JAC_form[breed_other_sheep], digits = 1) != all_JAC_form[breed_other_sheep]|
                                round(all_JAC_form[no_breed_other_sheep], digits = 1) != all_JAC_form[no_breed_other_sheep]|
                                round(all_JAC_form[lambs], digits = 1) != all_JAC_form[lambs]|
                                round(all_JAC_form[total_sheep], digits = 1) != all_JAC_form[total_sheep], 1, 0)

#Section 6 Poultry--------------------

total11 <- c(first_hens, moulted_hens, pullets, layer_chicks, table_chicks, cocks, broilers, turkeys, ducks, geese, other_poultry)
all_JAC_form[total11] <- all_JAC_form[total11] %>% mutate(across(where(is.numeric), ~  ifelse(is.na(.), 0, .)))
all_JAC_form$total11 <- rowSums(all_JAC_form[total11], na.rm =TRUE)
#err16 = err27 in SAS
all_JAC_form$err16 <- ifelse(round(all_JAC_form$total11, digits = 1) != round(all_JAC_form[total_poultry], digits = 1), 1, 0)
#err17 = err28 in SAS
all_JAC_form$err17 <- ifelse(round(all_JAC_form[first_hens], digits = 1) != all_JAC_form[first_hens]|
                               round(all_JAC_form[moulted_hens], digits = 1) != all_JAC_form[moulted_hens]|
                               round(all_JAC_form[pullets], digits = 1) != all_JAC_form[pullets]|
                               round(all_JAC_form[layer_chicks], digits = 1) != all_JAC_form[layer_chicks]|
                               round(all_JAC_form[table_chicks], digits = 1) != all_JAC_form[table_chicks]|
                               round(all_JAC_form[cocks], digits = 1) != all_JAC_form[cocks]|
                               round(all_JAC_form[broilers], digits = 1) != all_JAC_form[broilers]|
                               round(all_JAC_form[turkeys], digits = 1) != all_JAC_form[turkeys]|
                               round(all_JAC_form[ducks], digits = 1) != all_JAC_form[ducks]|
                               round(all_JAC_form[geese], digits = 1) != all_JAC_form[geese]|
                               round(all_JAC_form[other_poultry], digits = 1) != all_JAC_form[other_poultry]|
                               round(all_JAC_form[total_poultry], digits = 1) != all_JAC_form[total_poultry], 1, 0)
#err18 = err29 in SAS
all_JAC_form$err18 <- ifelse(round((all_JAC_form[first_hens] + all_JAC_form[moulted_hens] + all_JAC_form[pullets] + all_JAC_form[layer_chicks] +
                                     all_JAC_form[table_chicks] + all_JAC_form[broilers]), digits = 1) == 0 & round(all_JAC_form[cocks], digits = 1) > 9, 1, 0)


#Section 7 Miscellaneous Livestock--------------------
#err19 = err32 in SAS
all_JAC_form$err19 <- ifelse(round(all_JAC_form[deer], digits = 1) != all_JAC_form[deer], 1, 0)
#err22 = err35 in SAS
all_JAC_form$err22 <- ifelse(round(all_JAC_form[alpacas], digits = 1) != all_JAC_form[alpacas]|
                               round(all_JAC_form[llamas], digits = 1) != all_JAC_form[llamas]|
                               round(all_JAC_form[other_camelids], digits = 1) != all_JAC_form[other_camelids], 1, 0)
#err23 = err36 in SAS
all_JAC_form$err23 <- ifelse(round(all_JAC_form[hives], digits = 1) != all_JAC_form[hives], 1, 0)

#err24 = err37 in SAS
all_JAC_form$err24 <- ifelse(round(all_JAC_form[other_livestock], digits = 1) != all_JAC_form[other_livestock], 1, 0)

#err25 = err38 in SAS
all_JAC_form$err25 <- ifelse(all_JAC_form[other_livestock]>0 & is.na(all_JAC_form[other_livestock_txt]), 1, 0)

#items for and up to 2021 - not for 2023 (if running for 2023 and beyond comment this section out)-----------------------------------------------------------
#err20 = err33 in SAS
all_JAC_form$err20 <- ifelse(round(all_JAC_form[agri_horses], digits = 1) != all_JAC_form[agri_horses]|
                               round(all_JAC_form[other_horse_pony], digits = 1) != all_JAC_form[other_horse_pony]|
                               round(all_JAC_form[donkeys], digits = 1) != all_JAC_form[donkeys], 1, 0)

#err21 = err34 in SAS
all_JAC_form$err21 <- ifelse(round(all_JAC_form[female_goat_kids], digits = 1) != all_JAC_form[female_goat_kids]|
                               round(all_JAC_form[other_female_goat], digits = 1) != all_JAC_form[other_female_goat]|
                               round(all_JAC_form[other_goats_kids], digits = 1) != all_JAC_form[other_goats_kids], 1, 0)

#------------------------------------------------------------------------------------------------------------------------------------------------------------

#items for 2023 and beyond (if running for 2023 and beyond, uncomment this section and comment the above section out)--------------------------------------
#err20 = err33 in SAS
# all_JAC_form$err20 <- ifelse(round(all_JAC_form[donkeys], digits = 1) != all_JAC_form[donkeys]|
#                              round(all_JAC_form[horse_pony], digits = 1) != all_JAC_form[horse_pony], 1, 0)
# 
# #err21 = err34 in SAS
# all_JAC_form$err21 <- ifelse(round(all_JAC_form[goats_kids], digits = 1) != all_JAC_form[goats_kids], 1, 0)

#------------------------------------------------------------------------------------------------------------------------------------------------------
#Section 8 Legal Responsbility

#err26 = err39 in SAS
#item2726 doesn't exist?
# all_JAC_form$err26 <- ifelse(all_JAC_form[other_legal_return_21] ==1 & (all_JAC_form[occupier1_ft] + all_JAC_form[occupier1_pt_gthalf] +
#                                                                         all_JAC_form[occupier1_pt_lthalf] + all_JAC_form[occupier1_no_work] +
#                                                                           all_JAC_form[occupier2_ft] + all_JAC_form[occupier2_pt_gthalf] +
#                                                                           all_JAC_form[occupier2_pt_lthalf] + all_JAC_form[occupier2_no_work]) >0, 1, 0)

#------------------------------------------------------------------------------------------------------------------------------------------------------
#Section 9 Occupiers
#err27 = err40 in SAS

all_JAC_form$err27 <- ifelse(all_JAC_form[occupier1_ft] >=1 & all_JAC_form[occupier1_pt_gthalf] >=1|
                             all_JAC_form[occupier1_ft] >=1 & all_JAC_form[occupier1_pt_lthalf] >=1|
                               all_JAC_form[occupier1_pt_gthalf] >=1 & all_JAC_form[occupier1_pt_lthalf] >=1|
                               all_JAC_form[occupier1_ft] >=1 & all_JAC_form[occupier1_no_work] >=1|
                               all_JAC_form[occupier1_pt_gthalf] >=1 & all_JAC_form[occupier1_no_work] >=1|
                               #not in SAS code but should be (SAS erroneously repeats occupier1_ft and occupier1 no work)- adds 7 more observations to errors
                               all_JAC_form[occupier1_pt_lthalf] >=1 & all_JAC_form[occupier1_no_work] >=1, 1, 0) 

#err28 = err41 in SAS
all_JAC_form$err28 <- ifelse(all_JAC_form[occupier2_ft] >=1 & all_JAC_form[occupier2_pt_gthalf] >=1|
                               all_JAC_form[occupier2_ft] >=1 & all_JAC_form[occupier2_pt_lthalf] >=1|
                               all_JAC_form[occupier2_pt_gthalf] >=1 & all_JAC_form[occupier2_pt_lthalf] >=1|
                               all_JAC_form[occupier2_ft] >=1 & all_JAC_form[occupier2_no_work] >=1|
                               all_JAC_form[occupier2_pt_gthalf] >=1 & all_JAC_form[occupier2_no_work] >=1|
                               #not in SAS code but should be (SAS erroneously repeats occupier1_ft and occupier1 no work)- adds 5 more observations to errors
                               all_JAC_form[occupier2_pt_lthalf] >=1 & all_JAC_form[occupier2_no_work] >=1, 1, 0) 



#------------------------------------------------------------------------------------------------------------------------------------------------------------
#Section 10 #Other labour on the location
total12 <- c(labour_ft_m_bp, labour_ft_m_fam, labour_ft_m_hired, labour_ft_f_bp, labour_ft_f_fam, labour_ft_f_hired, 
             labour_pt_m_bp, labour_pt_f_bp, labour_pt_m_hired, labour_pt_m_fam, labour_pt_f_hired, labour_pt_f_fam,
             labour_cas_m, labour_cas_f)

all_JAC_form$total12 <- rowSums(all_JAC_form[total12], na.rm =TRUE)

#err29 = err42 in SAS
all_JAC_form$err29 <- ifelse(round(all_JAC_form[total_labour], digits = 1) != all_JAC_form$total12, 1, 0)

#err30 = err43 in SAS
all_JAC_form$err30 <- ifelse(round(all_JAC_form[labour_ft_m_bp], digits = 1) != all_JAC_form[labour_ft_m_bp]|
                               round(all_JAC_form[labour_ft_m_hired], digits = 1) != all_JAC_form[labour_ft_m_hired]|
                               round(all_JAC_form[labour_ft_m_fam], digits = 1) != all_JAC_form[labour_ft_m_fam]|
                               round(all_JAC_form[labour_ft_f_bp], digits = 1) != all_JAC_form[labour_ft_f_bp]|
                               round(all_JAC_form[labour_ft_f_hired], digits = 1) != all_JAC_form[labour_ft_f_hired]|
                               round(all_JAC_form[labour_ft_f_fam], digits = 1) != all_JAC_form[labour_ft_f_fam]|
                               round(all_JAC_form[labour_pt_m_bp], digits = 1) != all_JAC_form[labour_pt_m_bp]|
                               round(all_JAC_form[labour_pt_m_hired], digits = 1) != all_JAC_form[labour_pt_m_hired]|
                               round(all_JAC_form[labour_pt_m_fam], digits = 1) != all_JAC_form[labour_pt_m_fam]|
                               round(all_JAC_form[labour_pt_f_bp], digits = 1) != all_JAC_form[labour_pt_f_bp]|
                               round(all_JAC_form[labour_pt_f_hired], digits = 1) != all_JAC_form[labour_pt_f_hired]|
                               round(all_JAC_form[labour_pt_f_fam], digits = 1) != all_JAC_form[labour_pt_f_fam]|
                               round(all_JAC_form[labour_cas_m], digits = 1) != all_JAC_form[labour_cas_m]|
                               round(all_JAC_form[labour_cas_f], digits = 1) != all_JAC_form[labour_cas_f]|
                               round(all_JAC_form[total_labour], digits = 1) != all_JAC_form[total_labour], 1, 0)

#------------------------------------------------------------------------------------------------------------------------------------------------------
#err31 = err44 in SAS
#item2249 isn't in all_JAC_form?
all_JAC_form$err44 <- ifelse(all_JAC_form[owned_croft]> all_JAC_form[area_own], 1, 0)

#Crofting checks - to  finish 26/4/23 (SAS coding not clear...)
# all_JAC_f|m$err45 <- ifelse(all_JAC_f|m[owned_croft] > 0 & (parish <= 136 | parish <= 176 & parish <= 227 | parish == 231 | parish == 242 == parish == 273
#                                                               | 290 == parish == 432 | 466 == parish == 585 | 635 == parish == 735 
#                                                               | 769 == parish == 827 | 841 == parish == 868) )
# 
# 

