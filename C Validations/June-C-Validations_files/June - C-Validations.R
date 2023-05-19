#JAC Validations for Census form data (C2, C3 IN SAS)
#NB: when changing for 2023, area_rent_21 (item7) == area_rent (item20026)  




library(tidyverse)
#yr = this year 20xx
yr <- 2021
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

#item_lists----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#source("item_numbers.R") - can't use source when using markdown
#manual copy paste from "item_numbers.R" file - 17th May 2023
##Individual item numbers JAC 2023

#NB new item numbers for 2023 have 5 digits


#Section 1---------------------------------------------------------------------
#Area you own
area_own <- "item11"

#Area of land you rent in on a full tenancy at this location
area_rent <- "item20026"
area_rent_21 <- "item7"

#Total area
total_area <- "item12"


#Crofts
owned_croft <- "item2249"
rented_croft <- "item2251"

#Section 2---------------------------------------------------------------------
#Land seasonally rented in from another person- same location as last year
seas_rent_same_in <- "item2827"
#Land seasonally rented in from another person- did not rent this location last year
seas_rent_diff_in <- "item2828"
#total area of seasonal lets rented in
seas_total_area_in <- "item2"
#Land seasonally rented out to another person- area of land recorded in section 1 that you let out seasonally to another person
seas_area_rent_out <- "item1"

#Section 3---------------------------------------------------------------------

#Cereals and oilseeds - SAF only 
winter_barley <- "item16"
spring_barley <- "item18"
winter_oats <- "item17"
spring_oats <- "item20"
winter_rape <- "item19"
spring_rape <-  "item23"

#Cereals and oilseed - census form
all_barley <- "item27710"
all_oats <- "item27715"
wheat <- "item14"
all_rape <- "item27720"

#Potatoes 
#Potatoes intended mainly for seed
seed_potatoes <- "item24"
#Potatoes intended mainly for ware
ware_potatoes <- "item2320"


#Stockfeed  SAF- only
#Turnips and swede
turn_swed_sf <- "item29"
#maize
maize <- "item2059"
#other crops including cereal for silage
other_sf <- "item34"
#rape
rape_sf <- "item31"
#fodder beet
fbeet_sf <- "item32"
#Kale and cabbage
kale_cab_sf <- "item30"

#Stockfeed - census form
#All stockfeed – turnips, swedes, kale, rape, maize, fodder beet, silage crops
all_stockfeed <- "item27725"

#Veg for human consumption - SAF only
peas <- "item52"
beans <- "item53"
leeks <- "item55"
#turnips and swedes
turn_swed <- "item56"
#cabbages and savoys
cab_sav <- "item2323"
#Brussels Sprouts
brussel <- "item59"
#Calabrese/sprouting broccoli
calabrese <- "item60"
#cauliflowerand broccoli-heading varieties
cauli_broc <- "item61"
carrots <- "item63"
lettuce <- "item64"
rhubarb <- "item65"
#Other vegetables – area of individual crops which are too small to be sown separately
other_veg <- "item66"
#Total vegetables
all_veg_saf <- "item68"


#veg- census form
#All vegetables – e.g. peas, beans, leeks, turnips, carrots, lettuce etc.
all_veg_open <- "item27730"
all_veg_open_21 <- "item35"

#Fruit grown in open - SAF only
strawberries <- "item70"
raspberries <- "item71"
blackcurrants <- "item72"
blueberries <- "item2832"
orchard_fruit <- "item36"
mixed_soft_fruit <- "item75"
all_fruit_saf <- "item76"
soft_fruits <- "item37"

#fruit - census form
#All fruit – strawberries, raspberries, blackcurrant, blueberries, orchard etc.
all_fruit <- "item27735"

#flowers, bulbs and nursery stock grown in the open- SAF only

#Bulbs/flowers grown for the production of dry bulbs and/or cut flowers in the open
all_flow_bulb_SAF <- "item2324"
#Bedding and pot plants grown in the open
bed_pot_plant <- "item1709"
#Fruit stocks – spawn beds, runner beds, stool beds and young plants intended for sale
fruit_stocks <- "item80"
#Roses and rose stocks
roses_stocks <- "item81"
shrubs <- "item1710"
#Ornamental trees (not forest trees)
orn_trees <- "item82"
#Other nursery stock – herbaceous plants, alpines, etc. (not forest trees)
other_nurs <- "item83"

#flowers, bulbs and nursery stock grown in the open- census form
#All flowers, bulbs and nursery stocks
all_flow_bulb <- "item27740"
all_flow_bulb_21 <- "item84"

#glasshouses- SAF only
#Open soil
glass_tom_open <- "item2713"
glass_other_fruit_open <- "item2714"
glass_veg_open <- "item2715"
glass_bed_pot_plant_open <- "item2716"
glass_nurs_stock_open <- "item2717"
glass_strawb_open <- "item2858"
glass_rasp_open <- "item2859"
glass_blue_open <- "item2860"
glass_black_open <- "item2861"


#solid floor
glass_tom_solid <- "item2707"
glass_other_fruit_solid <-"item2708"
glass_veg_solid <-"item2709"
glass_bed_pot_plant_solid <-"item2710"
glass_nurs_stock_solid <- "item2711"
glass_strawb_solid <- "item2863"
glass_rasp_solid <- "item2864"
glass_blue_solid <- "item2865"
glass_black_solid <- "item2866"

#Walk in plastic structure greenhouse (Total open soil area)
tot_open_plastic <- "item85"
#Glass clad structure greenhouses (Total solid floor area)
tot_solid_glass <- "item86"

#Summary items for glasshouse crops (ie open soil item + solid floor item)
glass_tom_tot <- "item87"
glass_strawb_tot <- "item2556"
glass_rasp_tot <- "item2557"
glass_black_tot <- "item6000"
glass_blue_tot <- "item2836"
glass_other_fruit_tot <- "item2036"
glass_veg_tot <- "item2037"
glass_bed_pot_plant_tot <- "item1711"
glass_hardy_nurse_tot <- "item1943"
glass_unused_tot <- "item6001"

#glasshouses- census form
#All crops/plants
all_glass_open <- "item27750"
all_glass_solid <- "item27755"
#Unused area
glass_unused_solid <- "item2867"
glass_unused_open <- "item2862"
#other crops
other_crops <- "item41"
other_crops_text <- "item185"

#Fallow, Grass, Woodland, Other land- census form
#Fallow land for 5 years or less
fland_lte5 <- "item2469"
#Fallow land for more than 5 years
fland_gt5 <- "item2470"
#Grass under 5 years old (seeded or re-seeded in the last 5 years)
grass_lt5 <- "item2321"
#Grass 5 years old and older (not seeded or re-seeded in the last 5 years)
grass_gte5 <- "item2322"
#Rough grazings – mountain, hill, moor, deer forest situated within the farming unit, enclosed or not. Do not include woods, roads, share in common grazings or land taken seasonally
rough_graze <- "item47"
#Woodland – total area (ha.) of woodland (other than orchards) on the location, including woodland used for commercial or amenity purposes and forest trees
woodland <- "item48"
#Other land – roads, yards, buildings (excluding glasshouses), ponds, derelict land, etc.
other_land <- "item49"

#Total Area of Crops and Grass-SAF
total_crops_grass <- "item46"

#Total LAND 
#TOTAL LAND from Section 3 – should equal Total Area Section 1 box 3 (item12)
total_land <-  "item50"

#Section 4---------------------------------------------------------------------
#Pigs
#Sows in pig
sows_pig <- "item146"
#Gilts in pig
gilts_pig <- "item147"
#Other sows for breeding
other_sows_pig <- "item148"
#Barren sows for fattening
barren_sows_pig <- "item149"
#Gilts 50 kg and over, not yet in pig, but expected to be used for breeding
gilts_50_pig <- "item150" 
#Boars being used for service
boars_pig <- "item151"
#Fattening pigs (over 29 kg)
fat_pig <- "item27760"
#Weaners (29 kg or less)
wean_pig <- "item27765"
#Piglets/suckling pigs
piglet <- "item27770"
#total pigs-  items 146-156
total_pig <- "item157"

#----------items used up to 2021
#Other pigs 110kg and over
other_110_pig <- "item152"
#Other pigs 80-110kg
other_80_110_pig <- "item153"
#Other pigs 50-80kg
other_50_80_pig <- "item154"
#Other pigs 20-50kg
other_20_50_pig <- "item155"
#Other pigs under 20kg
other_lt20_pig <- "item156"

#Section 5---------------------------------------------------------------------
#Sheep
#Ewes that were used for breeding in 2023 – number still on farm at 1 June 20xx
ewes <- "item139"
#Rams to be used/expected to be used for service 
rams <- "item140"
#Other sheep 1 year and over used for breeding
breed_other_sheep <- "item141"
#Other sheep 1 year and over NOT used for breeding
no_breed_other_sheep <- "item143"
#Lambs
lambs <- "item144"
#Total sheep (items 139-141, 143-144)
total_sheep <- "item145"

#Section 6---------------------------------------------------------------------
#Poultry
#Fowls laying eggs for eating: hens in the laying stock: hens in the first laying season
first_hens <- "item158"
#Fowls laying eggs for eating: hens in the laying stock: moulted hens
moulted_hens <- "item159"
#Fowls laying eggs for eating: pullets being reared for laying
pullets <- "item161"
#Fowls of all ages for breeding: females laying eggs: to hatch layer chicks
layer_chicks <- "item160"
#Fowls of all ages for breeding: females laying eggs: to hatch table chicks
table_chicks <- "item162"
#Fowls of all ages for breeding: cocks
cocks <- "item163"
#Broilers and other table fowls
broilers <- "item164"
turkeys <- "item1708"
ducks <- "item2038"
geese <- "item2039"
other_poultry <- "item167"
#total poultry(item158-164, item167, item1708, item2038-2039)
total_poultry <- "item170"


#Section 7 ---------------------------------------------------------------------
#Miscellaneous Livestock
#Deer of all ages and types (exclude wild or park deer which cannot be gathered, identified, recorded and handled)
deer <- "item94"
donkeys <- "item2868"
alpacas <- "item2472"
llamas <- "item2473"
other_camelids <- "item2474"
#Hives containing a colony of live honey bees, whether owned by you or not
hives <- "item2826"
#Other farm livestock not previously mentioned – do not include cattle/buffalo here
other_livestock <- "item171"
#Specify type of livestock entered in box 9 (item171) above (please PRINT here)	
other_livestock_txt <- "item186"
#All horses and ponies
horse_pony <- "item27775"
#All goats and kids
goats_kids <- "item27780"

#-----------------items used up to 2021
#Horses Used in Agriculture
agri_horses <- "item95"
#All Other Horses and Ponies
other_horse_pony <- "item96"
#All Other Goats and kids
other_goats_kids <- "item98"
#Female Goats which have kidded this year or will kid this year
female_goat_kids <- "item1712"
#Other Female Goats which have been used for breeding in previous years
other_female_goat <- "item1713"

#Section 8 ---------------------------------------------------------------------
#Legal Responsibility
#Do not return your details on more than one location. If you have already returned your details on another 1 June 2023 census form, please tick this box and go on to Section 10
other_legal_return <- "item2727"
#Who has legal and financial responsibility for this location?
legal_fin_resp <- "item2980"
#Section 9 ---------------------------------------------------------------------
#Occupier(s)
#Occupier 1 details - legal responsibility is assumed
occupier1_male <-"item2877"
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

#Occupier 2 details - Occupier 2 – can include formalised relationships (married couples and civil partnerships), cohabiting couples and close family relationships (father, mother, daughter, son and sibling)
occupier2_male <-"item3056"
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

#-----------------items used up to 2021 (spouse = occupier 2 from 2023, item numbers remain the same)
#Do not return your details on more than one location form.If you have already returned your details on another 2018 census form, please tick this box and go on to Section 9
other_legal_return_21 <- "item2726"
# spouse_male <-"item3056"
# spouse_female <- "item3057"
# #spouse– proportion of time spent working on this location:
# #full-time
# spouse_ft <- "item182"
# #Part-time: half-time or more
# spouse_pt_gthalf <- "item183"
# #Part-time: less than half-time
# spouse_pt_lthalf <- "item184"
# #no farm work on this location
# spouse_no_work <- "item2567"

#Section 10 ---------------------------------------------------------------------
#Other labour on the location
#Full-time regular staff employed on 1 June 2023: Males:
#Business partners
labour_ft_m_bp <- "item1714"
labour_ft_m_hired <- "item1715"
#members of occupier's family
labour_ft_m_fam <- "item1716"

#Full-time regular staff employed on 1 June 2023: Females:
#Business partners
labour_ft_f_bp <- "item1717"
labour_ft_f_hired <- "item192"
#members of occupier's family
labour_ft_f_fam <- "item193"

#Part-time regular staff employed on 1 June 2023: Males:
#Business partners
labour_pt_m_bp <- "item1718"
labour_pt_m_hired <- "item194"
#members of occupier's family
labour_pt_m_fam <- "item195"

#Part-time regular staff employed on 1 June 2023: Females:
#Business partners
labour_pt_f_bp <- "item1719"
labour_pt_f_hired <- "item196"
#members of occupier's family
labour_pt_f_fam <- "item197"

#Casual and seasonal staff employed on 1 June 2023
#males
labour_cas_m <- "item198"
#females
labour_cas_f <- "item199"

#Total regular, casual and seasonal staff (Boxes 1 to 14 above -all items in section 10)
total_labour <- "item200"

#Section 11 --------------------------------------------------------------------- 
#Non-regular, contract labour and migrant workers
#Non-family labour employed on non-regular basis
labour_non_fam_non_reg <- "item2712"
#Agricultural contract labour (include workers who are employed by a third party)
labour_agri_contract <- "item2066"
#EU migrant workers (i.e. workers that are not UK nationals)- new for 2023
labour_EU_migrant <- "item2536"
#Non-EU migrant workers (i.e. workers that are not UK nationals) 
#Note that the definition of 2511 has changed (before 2023 = Migrant workers (i.e. workers that are not UK nationals)). For comparability, pre-2023 item2511 = newitem2511+newitem2536 
labour_non_EU_migrant <- "item2511"


#parishes NOT included on the Register of Crofts (downloaded 9/5/2023)
non_crofting_parishes <- c(0:136, 176:227, 231, 242:273, 290:432, 466:585, 635:735, 769:827, 841:868)



# Import ------------------------------------------------------------------

#combined dataset (SAF and JAC data)
SAS_combined <- read.csv(paste0(output_path, "COMBINED_DATA.csv"))
SAS_combined <- SAS_combined %>% group_by(parish, holding)

#croft dataset
load(paste0(output_path, "crofts_A.rda"))
croft <- df_crofts %>% group_by(parish, holding)


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
#err1 = err2 in SAS = match (1740)

#Total area not equal to area rent + area own
all_JAC_form$err1 <- ifelse(round(all_JAC_form[total_area], digits = 2) != round(all_JAC_form[area_own]+all_JAC_form[area_rent_21], digits = 2),  1, 0)

#Total area is zero but area own or area rented >0
all_JAC_form$err37 <- ifelse((all_JAC_form[total_area] == 0 |  is.na(all_JAC_form[total_area])) & (all_JAC_form[area_own]| all_JAC_form[area_rent_21] > 0), 1, 0)

#Total area is zero and area owned and area rented =0
all_JAC_form$err38 <- ifelse((all_JAC_form[total_area] == 0 |  is.na(all_JAC_form[total_area])) & 
                               (all_JAC_form[area_own] == 0 |  is.na(all_JAC_form[area_own])) & 
                               all_JAC_form[area_rent_21] == 0 |  is.na(all_JAC_form[area_rent_21]), 1, 0)


# Section 2 Seasonal rents--------------------

#err2 = err10 in SAS = no match (15 vs 18 in SAS)
#Total seasonal rent area = area rent in section 1 
all_JAC_form$err2 <- ifelse(round(all_JAC_form[seas_total_area_in], digits = 2) == round(all_JAC_form[area_rent_21], digits = 2)
                            & (all_JAC_form[seas_total_area_in] & all_JAC_form[area_rent_21]> 0) & all_JAC_form$land_data == "ags",  1, 0)

#err3 = err11 in SAS = match (20)
#new seasonal rent + same location seasonal rent = total seasonal rent area
all_JAC_form$err3 <- ifelse(round(all_JAC_form[seas_rent_diff_in] + all_JAC_form[seas_rent_same_in], digits = 2) != round(all_JAC_form[seas_total_area_in], digits = 2)
                            & all_JAC_form$land_data != "both",  1, 0)


# Section 3 Land Use --------------------------

#Validate/QA SAF totals (adding itemised crops) vs form summary totals?

#name of other crop given if area > 100 ha
#err4 = err13 in SAS
all_JAC_form$err4 <-  ifelse(all_JAC_form[other_crops] > 100 & is.na(all_JAC_form[other_crops_text]),  1, 0)


#grassland-------------------------------------------------------------------------------------------------------------------------------------------------
#err5 = err15 in SAS = no match (1065 vs 1066 in SAS)
total7 <- c(total_crops_grass, rough_graze, woodland, other_land)
all_JAC_form$total7 <- rowSums(all_JAC_form[total7], na.rm =TRUE)

all_JAC_form$err5 <-  
  ifelse(all_JAC_form$land_data == "ags", ifelse(round(all_JAC_form[total_land], digits = 2) != round(all_JAC_form$total7, digits = 2), 1, 0), 0)

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

#glasshouses

#err11 = err23 in SAS = no match (23 vs 25 in SAS)
total5 <- c(glass_tom_open, glass_other_fruit_open, glass_veg_open, glass_bed_pot_plant_open, 
            glass_nurs_stock_open, glass_strawb_open, glass_rasp_open, glass_blue_open, glass_black_open, glass_unused_open)
total6 <- c(glass_tom_solid, glass_other_fruit_solid, glass_veg_solid, glass_bed_pot_plant_solid,
            glass_nurs_stock_solid, glass_strawb_solid, glass_rasp_solid, glass_blue_solid, glass_black_solid, glass_unused_solid)

all_JAC_form$total5 <- rowSums(all_JAC_form[total5], na.rm =TRUE)

all_JAC_form$total6 <- rowSums(all_JAC_form[total6], na.rm =TRUE)

all_JAC_form$err11<-  
  ifelse(all_JAC_form$survtype == "Non-SAF", ifelse((round(all_JAC_form$total5, digits = 2) != round(all_JAC_form[tot_open_plastic], digits = 2))| 
                                                      (round(all_JAC_form$total6, digits = 2) != round(all_JAC_form[tot_solid_glass], digits = 2)),  1, 0), 0)

#err12 = err24 in SAS = match (15)
all_JAC_form$err12 <- ifelse(all_JAC_form[tot_open_plastic]+all_JAC_form[tot_solid_glass]>0 & (all_JAC_form$total5 + all_JAC_form$total6)<=0, 1, 0 )



#2023 version - not itemised, using summary values. If SAF data use above code 
#total5b <- c(all_glass_open, glass_unused_open)
#total6b <- c(all_glass_solid, glass_unused_solid)
#all_JAC_form$total5b <- rowSums(all_JAC_form[total5b], na.rm =TRUE)
#all_JAC_form$total6b <- rowSums(all_JAC_form[total6b], na.rm =TRUE)
# all_JAC_form$err11<-  
#   ifelse((round(all_JAC_form$total5b, digits = 2) != round(all_JAC_form[tot_open_plastic], digits = 2))| 
#                                                       (round(all_JAC_form$total6b, digits = 2) != round(all_JAC_form[tot_solid_glass], digits = 2)),  1, 0)

#all_JAC_form$err12 <- ifelse(all_JAC_form[tot_open_plastic]+all_JAC_form[tot_solid_glass]>0 & (all_JAC_form$total5b + all_JAC_form$total6b)<=0, 1, 0 )




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
#err17 = err27 in SAS = match (1)
all_JAC_form$err17 <- ifelse(round(all_JAC_form$total11, digits = 1) != round(all_JAC_form[total_poultry], digits = 1), 1, 0)
#err18 = err28 in SAS =
all_JAC_form$err18 <- ifelse(round(all_JAC_form[first_hens], digits = 1) != all_JAC_form[first_hens]|
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
#err19 = err29 in SAS = match (2)
all_JAC_form$err19 <- ifelse(round((all_JAC_form[first_hens] + all_JAC_form[moulted_hens] + all_JAC_form[pullets] + all_JAC_form[layer_chicks] +
                                     all_JAC_form[table_chicks] + all_JAC_form[broilers]), digits = 1) == 0 & round(all_JAC_form[cocks], digits = 1) > 9, 1, 0)


#Section 7 Miscellaneous Livestock--------------------
#err20 = err32 in SAS
all_JAC_form$err20 <- ifelse(round(all_JAC_form[deer], digits = 1) != all_JAC_form[deer], 1, 0)
#err21 = err35 in SAS
all_JAC_form$err21 <- ifelse(round(all_JAC_form[alpacas], digits = 1) != all_JAC_form[alpacas]|
                               round(all_JAC_form[llamas], digits = 1) != all_JAC_form[llamas]|
                               round(all_JAC_form[other_camelids], digits = 1) != all_JAC_form[other_camelids], 1, 0)
#err22 = err36 in SAS
all_JAC_form$err22 <- ifelse(round(all_JAC_form[hives], digits = 1) != all_JAC_form[hives], 1, 0)

#err23 = err37 in SAS
all_JAC_form$err23 <- ifelse(round(all_JAC_form[other_livestock], digits = 1) != all_JAC_form[other_livestock], 1, 0)

#err24 = err38 in SAS
all_JAC_form$err24 <- ifelse(all_JAC_form[other_livestock]>0 & is.na(all_JAC_form[other_livestock_txt]), 1, 0)

#items for and up to 2021 - not for 2023 (if running for 2023 and beyond comment this section out)-----------------------------------------------------------
#err25 = err33 in SAS
all_JAC_form$err25 <- ifelse(round(all_JAC_form[agri_horses], digits = 1) != all_JAC_form[agri_horses]|
                               round(all_JAC_form[other_horse_pony], digits = 1) != all_JAC_form[other_horse_pony]|
                               round(all_JAC_form[donkeys], digits = 1) != all_JAC_form[donkeys], 1, 0)

#err26 = err34 in SAS
all_JAC_form$err26 <- ifelse(round(all_JAC_form[female_goat_kids], digits = 1) != all_JAC_form[female_goat_kids]|
                               round(all_JAC_form[other_female_goat], digits = 1) != all_JAC_form[other_female_goat]|
                               round(all_JAC_form[other_goats_kids], digits = 1) != all_JAC_form[other_goats_kids], 1, 0)

#------------------------------------------------------------------------------------------------------------------------------------------------------------

#items for 2023 and beyond (if running for 2023 and beyond, uncomment this section and comment the above section out)--------------------------------------
#err27 = err33 in SAS
# all_JAC_form$err27 <- ifelse(round(all_JAC_form[donkeys], digits = 1) != all_JAC_form[donkeys]|
#                              round(all_JAC_form[horse_pony], digits = 1) != all_JAC_form[horse_pony], 1, 0)
# 
# #err28 = err34 in SAS
# all_JAC_form$err28 <- ifelse(round(all_JAC_form[goats_kids], digits = 1) != all_JAC_form[goats_kids], 1, 0)

#------------------------------------------------------------------------------------------------------------------------------------------------------
#Section 8 Legal Responsbility

#err29 = err39 in SAS
#item2726 doesn't exist?
# all_JAC_form$err29 <- ifelse(all_JAC_form[other_legal_return_21] ==1 & (all_JAC_form[occupier1_ft] + all_JAC_form[occupier1_pt_gthalf] +
#                                                                         all_JAC_form[occupier1_pt_lthalf] + all_JAC_form[occupier1_no_work] +
#                                                                           all_JAC_form[occupier2_ft] + all_JAC_form[occupier2_pt_gthalf] +
#                                                                           all_JAC_form[occupier2_pt_lthalf] + all_JAC_form[occupier2_no_work]) >0, 1, 0)

#------------------------------------------------------------------------------------------------------------------------------------------------------
#Section 9 Occupiers
#err30 = err40 in SAS = no match (143 vs 136 in SAS)

all_JAC_form$err30 <- ifelse(all_JAC_form[occupier1_ft] >=1 & all_JAC_form[occupier1_pt_gthalf] >=1|
                             all_JAC_form[occupier1_ft] >=1 & all_JAC_form[occupier1_pt_lthalf] >=1|
                               all_JAC_form[occupier1_pt_gthalf] >=1 & all_JAC_form[occupier1_pt_lthalf] >=1|
                               all_JAC_form[occupier1_ft] >=1 & all_JAC_form[occupier1_no_work] >=1|
                               all_JAC_form[occupier1_pt_gthalf] >=1 & all_JAC_form[occupier1_no_work] >=1|
                               #not in SAS code but should be (SAS erroneously repeats occupier1_ft and occupier1 no work)- adds 7 more observations to errors
                               all_JAC_form[occupier1_pt_lthalf] >=1 & all_JAC_form[occupier1_no_work] >=1, 1, 0) 

#err31 = err41 in SAS = no match (28 vs 23 in SAS)
all_JAC_form$err31 <- ifelse(all_JAC_form[occupier2_ft] >=1 & all_JAC_form[occupier2_pt_gthalf] >=1|
                               all_JAC_form[occupier2_ft] >=1 & all_JAC_form[occupier2_pt_lthalf] >=1|
                               all_JAC_form[occupier2_pt_gthalf] >=1 & all_JAC_form[occupier2_pt_lthalf] >=1|
                               all_JAC_form[occupier2_ft] >=1 & all_JAC_form[occupier2_no_work] >=1|
                               all_JAC_form[occupier2_pt_gthalf] >=1 & all_JAC_form[occupier2_no_work] >=1|
                               #not in SAS code but should be (SAS erroneously repeats occupier1_ft and occupier1 no work)- adds 5 more observations to errors
                               all_JAC_form[occupier2_pt_lthalf] >=1 & all_JAC_form[occupier2_no_work] >=1, 1, 0) 

#------------------------------------------------------------------------------------------------------------------------------------------------------------
#Section 10 Other labour on the location
total12 <- c(labour_ft_m_bp, labour_ft_m_fam, labour_ft_m_hired, labour_ft_f_bp, labour_ft_f_fam, labour_ft_f_hired, 
             labour_pt_m_bp, labour_pt_f_bp, labour_pt_m_hired, labour_pt_m_fam, labour_pt_f_hired, labour_pt_f_fam,
             labour_cas_m, labour_cas_f)

all_JAC_form$total12 <- rowSums(all_JAC_form[total12], na.rm =TRUE)

#err32 = err42 in SAS = match (4)
all_JAC_form$err32 <- ifelse(round(all_JAC_form[total_labour], digits = 1) != all_JAC_form$total12, 1, 0)

#err33 = err43 in SAS
all_JAC_form$err33 <- ifelse(round(all_JAC_form[labour_ft_m_bp], digits = 1) != all_JAC_form[labour_ft_m_bp]|
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
#err34 = err44 in SAS
#item2249 isn't in all_JAC_form?
#all_JAC_form$err34 <- ifelse(all_JAC_form[owned_croft]> all_JAC_form[area_own], 1, 0)

#err35 = err3 in SAS
#item2249 doesn't exist
#all_JAC_form$err35 <- ifelse (all_JAC_form[owned_croft] > 0 & all_JAC_form$parish %in% non_crofting_parishes, 1, 0)

#err36 = err4 in SAS 
all_JAC_form$err36 <- ifelse (all_JAC_form[rented_croft] > 0 & all_JAC_form$parish %in% non_crofting_parishes, 1, 0)


#migrant worker suppresssion - need to write function

#Migrant worker check (C4)---------------------------------------------------------------------------------------

d15 <- c(glass_tom_tot, glass_other_fruit_tot, glass_veg_tot, glass_strawb_tot, glass_rasp_tot, glass_blue_tot, glass_black_tot)
g01a <- c(orchard_fruit, raspberries, blackcurrants, blueberries, mixed_soft_fruit )

fruit_holdings <- SAS_combined
fruit_holdings$d15 <- rowSums(fruit_holdings[d15], na.rm =TRUE)
fruit_holdings$g01a <- rowSums(fruit_holdings[g01a], na.rm =TRUE)
fruit_holdings$sumfruit <- fruit_holdings$d15 + fruit_holdings$g01a

fruit_holdings$slr_d15 <- 7000.0
fruit_holdings$slr_g01 <- 425.0
fruit_holdings$slr_fruit <- ((fruit_holdings$slr_d15*fruit_holdings$d15) + (fruit_holdings$slr_g01*fruit_holdings$g01a)) / 1900


fruit_holdings$ftocc1 <- ifelse(fruit_holdings[occupier1_ft] >0, 1, 0)
fruit_holdings$ptocc1 <- ifelse(fruit_holdings[occupier1_pt_gthalf] >0, 1, 0)
fruit_holdings$lptocc1 <- ifelse(fruit_holdings[occupier1_pt_lthalf] >0, 1, 0)
fruit_holdings$nwrkocc1 <- ifelse(fruit_holdings[occupier1_no_work] >0, 1, 0)
fruit_holdings$ftocc2 <- ifelse(fruit_holdings[occupier2_ft] >0, 1, 0)
fruit_holdings$ptocc2 <- ifelse(fruit_holdings[occupier2_pt_gthalf] >0, 1, 0)
fruit_holdings$lptocc2 <- ifelse(fruit_holdings[occupier2_pt_lthalf] >0, 1, 0)
fruit_holdings$nwrkocc2 <- ifelse(fruit_holdings[occupier2_no_work] >0, 1, 0)

full <- c(labour_ft_m_bp, labour_ft_m_hired, labour_ft_f_bp, labour_ft_f_hired)
fruit_holdings$full <- rowSums(fruit_holdings[full], na.rm = TRUE)
fruit_holdings$full <- fruit_holdings$ftocc1 + fruit_holdings$ftocc2 + fruit_holdings$full

part <-  c(labour_pt_m_bp, labour_pt_m_hired, labour_pt_f_bp, labour_pt_f_hired)
fruit_holdings$part <- rowSums(fruit_holdings[part], na.rm = TRUE)
fruit_holdings$part <- fruit_holdings$ptocc1 + fruit_holdings$ptocc2 + fruit_holdings$part


casual <- c(labour_cas_m, labour_cas_f)
fruit_holdings$casual <- rowSums(fruit_holdings[casual], na.rm = TRUE)
fruit_holdings$casual <- fruit_holdings$lptocc1 + fruit_holdings$lptocc2 + fruit_holdings$casual

nonreg <- c(labour_non_fam_non_reg, labour_agri_contract, labour_non_EU_migrant)
fruit_holdings$nonreg <- rowSums(fruit_holdings[nonreg], na.rm = TRUE)

#2023
#nonreg <- fruit_holdings[labour_non_fam_non_reg] + fruit_holdings[labour_agri_contract] + fruit_holdings[labour_non_EU_migrant] +fruit_holdings[labour_EU_migrant]

fruit_holdings$full_lab <- fruit_holdings$full
fruit_holdings$part_lab <- fruit_holdings$part*0.5
fruit_holdings$casual_lab <- fruit_holdings$casual*0.25
fruit_holdings$nonreg_lab <- fruit_holdings$nonreg*8/1900
fruit_holdings$lab_returned <- (fruit_holdings$full_lab + fruit_holdings$part_lab + fruit_holdings$casual_lab + fruit_holdings$nonreg_lab)
fruit_holdings$lab_returned[is.na(fruit_holdings$lab_returned)] <- 0

#Sum labour by brn
brn_fruit <- fruit_holdings %>% ungroup() %>% select(brn, sumfruit, slr_fruit, lab_returned) %>% group_by(brn) %>% filter(brn >0 ) %>% arrange(brn) 
brn_fruit<- brn_fruit %>% summarise(freq =n(), sumfruit_sum = sum(sumfruit), slr_fruit_sum = sum(slr_fruit), lab_returned_sum = sum(lab_returned))

#holdings with more than 2ha of fruit
large_fruit <-fruit_holdings %>% filter(sumfruit>2 & survtype != "SAF only") %>% arrange(brn, by_group = FALSE)

check_labour <- left_join(large_fruit, brn_fruit, by = "brn" ) 

check_labour <- check_labour %>% mutate(diff_lab = slr_fruit_sum - lab_returned_sum,
                                        diff_lab_nobrn = slr_fruit- lab_returned) %>% arrange(desc(diff_lab))

check_labour <- check_labour %>%  select(parish, holding, survtype, brn, land_data, saf_data, sumfruit, sumfruit_sum, slr_fruit, slr_fruit_sum, lab_returned, lab_returned_sum, diff_lab, diff_lab_nobrn )

#Migrant check errors
check_labour$err39 <- ifelse(ifelse(check_labour$brn>=0, abs(check_labour$diff_lab) > 30 & check_labour$lab_returned_sum < 0.3*check_labour$slr_fruit_sum, 0), 1, 0)
check_labour$err40 <- ifelse(ifelse(check_labour$brn>=0, abs(check_labour$diff_lab) > 30 & check_labour$lab_returned_sum > 3*check_labour$slr_fruit_sum, 0), 1, 0)
check_labour$err41 <- ifelse(ifelse(check_labour$brn<0 | is.na(check_labour$brn), abs(check_labour$diff_lab_nobrn) > 30 & check_labour$lab_returned < 0.3*check_labour$slr_fruit, 0), 1, 0)
check_labour$err42 <- ifelse(ifelse(check_labour$brn<0 | is.na(check_labour$brn), abs(check_labour$diff_lab_nobrn) > 30 & check_labour$lab_returned > 3*check_labour$slr_fruit, 0), 1, 0)

#Validation Outputs---------------------------------------------------------------------------------------
#dataframe of observations (holdings) that have failed any of the validations
JAC_validation_errors <- all_JAC_form %>% filter(if_any(starts_with("err"), ~ . !=0))


#dataframe of observations (holdings) that have passed validations
clean_JAC <- all_JAC_form %>% filter(if_all(starts_with("err"), ~ . ==0 )) %>% select(-matches("err|total"))

#dataframe containing summary totals of errors
#2838 (SAS) vs 2774 (R)
JAC_validation_error_summary <- JAC_validation_errors%>% ungroup() %>%  select(starts_with("err")) %>% summarize(across(everything(), sum, na.rm = TRUE))
JAC_validation_error_summary <- cbind(JAC_validation_error_summary, total_cases_with_errors = nrow(JAC_validation_errors))
rownames(JAC_validation_error_summary) <- "errors"
JAC_validation_error_summary <- JAC_validation_error_summary %>% pivot_longer(cols= everything(), names_to = "error", values_to = "count")
JAC_validation_error_summary <- JAC_validation_error_summary %>% filter(count !=0)



#R Markdown tables
##----err 1----------------------------------------------------------------------------------------------
##@knitr err1

err1 <- all_JAC_form %>% select(parish, holding, survtype,  all_of(c(area_own, area_rent_21, total_area)), err1)     
err1 <-err1 %>%  mutate(error=ifelse(err1== 1, "Error", "No Error")) %>% filter(error == "Error")

err37 <- all_JAC_form %>% select(parish, holding, survtype,  all_of(c(area_own, area_rent_21, total_area)), err37)     
err37 <-err37 %>%  mutate(error=ifelse(err37== 1, "Error", "No Error")) %>% filter(error == "Error")

err38 <- all_JAC_form %>% select(parish, holding, survtype,  all_of(c(area_own, area_rent_21, total_area)), err38)     
err38 <-err38 %>%  mutate(error=ifelse(err38== 1, "Error", "No Error")) %>% filter(error == "Error")


##----err 2----------------------------------------------------------------------------------------------
##@knitr err2
#change area_rent_21 to area_rent for 2023
err2 <- all_JAC_form %>% select(parish, holding, survtype,  all_of(c(seas_total_area_in, area_rent_21)), err2)     
err2 <-err2 %>%  mutate(error=ifelse(err2== 1, "Error", "No Error")) %>% filter(error == "Error")

##----err 3----------------------------------------------------------------------------------------------
##@knitr err3

err3 <- all_JAC_form %>% select(parish, holding, survtype,  all_of(c(seas_total_area_in, seas_rent_diff_in, seas_rent_same_in)), err3)     
err3 <-err3 %>%  mutate(error=ifelse(err3== 1 & !is.na(err3), "Error", "No Error"))%>% filter(error == "Error")


##----err 4----------------------------------------------------------------------------------------------
##@knitr err4

err4 <- all_JAC_form %>% select(parish, holding, survtype,  all_of(c(other_crops, other_crops_text)), err4)     
err4 <-err4 %>%  mutate(error=ifelse(err4== 1 & !is.na(err4), "Error", "No Error"))%>% filter(error == "Error")


##----err 5----------------------------------------------------------------------------------------------
##@knitr err5

err5 <- all_JAC_form %>% select(parish, holding, survtype,  all_of(c(total_crops_grass, rough_graze, woodland, other_land)), total7, err5)     
err5 <-err5 %>%  mutate(error=ifelse(err5== 1 & !is.na(err5), "Error", "No Error"))%>% filter(error == "Error")

##----err 6----------------------------------------------------------------------------------------------
##@knitr err6
#err6 <- all_JAC_form %>% select(parish, holding, survtype,  all_of(c(total2)), total2, err6)     
#err6 <-err6 %>%  mutate(error=ifelse(err6== 1 & !is.na(err6), "Error", "No Error"))%>% filter(error == "Error")

##----err 7----------------------------------------------------------------------------------------------
##@knitr err7
#change all_veg_open_21 to all_veg_open for 2023
err7 <- all_JAC_form %>% select(parish, holding, survtype,  all_of(c(all_veg_saf, all_veg_open_21)), err7)     
err7 <-err7 %>%  mutate(error=ifelse(err7== 1 & !is.na(err7), "Error", "No Error"))%>% filter(error == "Error")

##----err 8----------------------------------------------------------------------------------------------
##@knitr err8

#err8 <- all_JAC_form %>% select(parish, holding, survtype,  all_of(c(all_fruit_saf, total3)), total3, err8)     
#err8 <-err8 %>%  mutate(error=ifelse(err8== 1 & !is.na(err8), "Error", "No Error"))%>% filter(error == "Error")

##----err 9----------------------------------------------------------------------------------------------
##@knitr err9
err9 <- all_JAC_form %>% select(parish, holding, survtype,  all_of(c(soft_fruits, all_fruit_saf)), err9)     
err9 <-err9 %>%  mutate(error=ifelse(err9== 1 & !is.na(err9), "Error", "No Error"))%>% filter(error == "Error")

##----err 10----------------------------------------------------------------------------------------------
##@knitr err10
#change all_flow_bulb_21 to all_flow_bulb for 2023
#err10 <- all_JAC_form %>% select(parish, holding, survtype,  all_of(c(all_flow_bulb_21, total4)), err10)     
#err10 <-err10 %>%  mutate(error=ifelse(err10== 1 & !is.na(err10), "Error", "No Error"))%>% filter(error == "Error")

##----err 11----------------------------------------------------------------------------------------------
##@knitr err11
err11 <- all_JAC_form %>% select(parish, holding, survtype,  all_of(c(total5, tot_open_plastic, total6, tot_solid_glass)), err11)     
err11 <-err11 %>%  mutate(error=ifelse(err11== 1 & !is.na(err11), "Error", "No Error"))%>% filter(error == "Error")

##----err 12----------------------------------------------------------------------------------------------
##@knitr err12
err12 <- all_JAC_form %>% select(parish, holding, survtype,  all_of(c(total5, tot_open_plastic, total6, tot_solid_glass)), err12)     
err12 <-err12 %>%  mutate(error=ifelse(err12== 1 & !is.na(err12), "Error", "No Error"))%>% filter(error == "Error")

##----err 13----------------------------------------------------------------------------------------------
##@knitr err13
#change total10_21 to total10 for 2023
err13 <- all_JAC_form %>% select(parish, holding, survtype,  all_of(c(total10_21, total_pig)), err13)     
err13 <-err13 %>%  mutate(error=ifelse(err13== 1 & !is.na(err13), "Error", "No Error"))%>% filter(error == "Error")

##----err 14----------------------------------------------------------------------------------------------
##@knitr err14
#change total10_21 to total10 for 2023
err14 <- all_JAC_form %>% select(parish, holding, survtype,  all_of(c(total10_21)), err14)     
err14 <-err14 %>%  mutate(error=ifelse(err14== 1 & !is.na(err14), "Error", "No Error"))%>% filter(error == "Error")

##----err 15----------------------------------------------------------------------------------------------
##@knitr err15
err15 <- all_JAC_form %>% select(parish, holding, survtype,  all_of(c(total9, total_sheep)), total9, err15)     
err15 <-err15 %>%  mutate(error=ifelse(err15== 1 & !is.na(err15), "Error", "No Error"))%>% filter(error == "Error")

##----err 16----------------------------------------------------------------------------------------------
##@knitr err16
err16 <- all_JAC_form %>% select(parish, holding, survtype,  all_of(c(total9, total_sheep)), total9, err16)     
err16 <-err16 %>%  mutate(error=ifelse(err16== 1 & !is.na(err16), "Error", "No Error"))%>% filter(error == "Error")

##----err 17----------------------------------------------------------------------------------------------
##@knitr err17
err17 <- all_JAC_form %>% select(parish, holding, survtype,  all_of(c(total11, total_poultry)), total11, err17)     
err17 <-err17 %>%  mutate(error=ifelse(err17== 1 & !is.na(err17), "Error", "No Error"))%>% filter(error == "Error")

##----err 18----------------------------------------------------------------------------------------------
##@knitr err18
err18 <- all_JAC_form %>% select(parish, holding, survtype,  all_of(c(total11, total_poultry)), err18)     
err18 <-err18 %>%  mutate(error=ifelse(err18== 1 & !is.na(err18), "Error", "No Error"))%>% filter(error == "Error")

##----err 19----------------------------------------------------------------------------------------------
##@knitr err19
err19 <- all_JAC_form %>% select(parish, holding, survtype,  all_of(c(first_hens, moulted_hens, pullets, layer_chicks, table_chicks, cocks, broilers)), err19)     
err19 <-err19 %>%  mutate(error=ifelse(err19== 1 & !is.na(err19), "Error", "No Error"))%>% filter(error == "Error")

##----err 20----------------------------------------------------------------------------------------------
##@knitr err20
err20 <- all_JAC_form %>% select(parish, holding, survtype,  all_of(deer), err20)     
err20 <-err20 %>%  mutate(error=ifelse(err20== 1 & !is.na(err20), "Error", "No Error"))%>% filter(error == "Error")

##----err 21----------------------------------------------------------------------------------------------
##@knitr err21
err21 <- all_JAC_form %>% select(parish, holding, survtype,  all_of(c(alpacas, llamas, other_camelids)), err21)     
err21 <-err21 %>%  mutate(error=ifelse(err21== 1 & !is.na(err21), "Error", "No Error"))%>% filter(error == "Error")

##----err 22----------------------------------------------------------------------------------------------
##@knitr err22
err22 <- all_JAC_form %>% select(parish, holding, survtype,  all_of(c(hives)), err22)     
err22 <-err22 %>%  mutate(error=ifelse(err22== 1 & !is.na(err22), "Error", "No Error"))%>% filter(error == "Error")

##----err 23----------------------------------------------------------------------------------------------
##@knitr err23
err23 <- all_JAC_form %>% select(parish, holding, survtype,  all_of(c(other_livestock)), err23)  
err23 <-err23 %>%  mutate(error=ifelse(err23== 1 & !is.na(err23), "Error", "No Error"))%>% filter(error == "Error")

##----err 24----------------------------------------------------------------------------------------------
##@knitr err24
err24 <- all_JAC_form %>% select(parish, holding, survtype,  all_of(c(other_livestock, other_livestock_txt)), err24)  
err24 <-err24 %>%  mutate(error=ifelse(err24== 1 & !is.na(err24), "Error", "No Error"))%>% filter(error == "Error")

##----err 25----------------------------------------------------------------------------------------------
##@knitr err25
err25 <- all_JAC_form %>% select(parish, holding, survtype,  all_of(c(agri_horses, other_horse_pony, donkeys)), err25)  
err25 <-err25 %>%  mutate(error=ifelse(err25== 1 & !is.na(err25), "Error", "No Error"))%>% filter(error == "Error")

##----err 26----------------------------------------------------------------------------------------------
##@knitr err26
err26 <- all_JAC_form %>% select(parish, holding, survtype,  all_of(c(female_goat_kids, other_female_goat, other_goats_kids)), err26)  
err26 <-err26 %>%  mutate(error=ifelse(err26== 1 & !is.na(err26), "Error", "No Error"))%>% filter(error == "Error")

##----err 27----------------------------------------------------------------------------------------------
##@knitr err27
#err27 <- all_JAC_form %>% select(parish, holding, survtype,  all_of(c(horse_pony, donkeys)), err27)  
#err27 <-err27 %>%  mutate(error=ifelse(err27== 1 & !is.na(err27), "Error", "No Error"))%>% filter(error == "Error")

# ##----err 28----------------------------------------------------------------------------------------------
# ##@knitr err28
# err28 <- all_JAC_form %>% select(parish, holding, survtype,  all_of(c(goats_kids)), err28)  
# err28 <-err28 %>%  mutate(error=ifelse(err28== 1 & !is.na(err28), "Error", "No Error"))%>% filter(error == "Error")

##----err 29----------------------------------------------------------------------------------------------
##@knitr err29
#err29 <- all_JAC_form %>% select(parish, holding, survtype,  all_of(c(other_legal_return_21, occupier1_ft, occupier1_pt_gthalf, occupier1_pt_lthalf, occupier1_no_work,
#                                                                      occupier2_ft, occupier2_pt_gthalf, occupier2_pt_lthalf, occupier2_no_work)), err29)  
#err29 <-err29 %>%  mutate(error=ifelse(err29== 1 & !is.na(err29), "Error", "No Error"))%>% filter(error == "Error")



##----err 30----------------------------------------------------------------------------------------------
##@knitr err29
err30 <- all_JAC_form %>% select(parish, holding, survtype,  all_of(c(occupier1_ft, occupier1_pt_gthalf, occupier1_pt_lthalf, occupier1_no_work)), err30)  
err30 <-err30 %>%  mutate(error=ifelse(err30== 1 & !is.na(err30), "Error", "No Error"))%>% filter(error == "Error")

##----err 31----------------------------------------------------------------------------------------------
##@knitr err31
err31 <- all_JAC_form %>% select(parish, holding, survtype,  all_of(c(occupier2_ft, occupier2_pt_gthalf, occupier2_pt_lthalf, occupier2_no_work)), err31)  
err31 <-err31 %>%  mutate(error=ifelse(err31== 1 & !is.na(err31), "Error", "No Error"))%>% filter(error == "Error")


##----err 32----------------------------------------------------------------------------------------------
##@knitr err32
err32 <- all_JAC_form %>% select(parish, holding, survtype,  all_of(c(total12, total_labour)), total12, err32)  
err32 <-err32 %>%  mutate(error=ifelse(err32== 1 & !is.na(err32), "Error", "No Error"))%>% filter(error == "Error")


##----err 33----------------------------------------------------------------------------------------------
##@knitr err33
err33 <- all_JAC_form %>% select(parish, holding, survtype,  all_of(c(total12, total_labour)), err33)  
err33 <-err33 %>%  mutate(error=ifelse(err33== 1 & !is.na(err33), "Error", "No Error"))%>% filter(error == "Error")


# ##----err 34----------------------------------------------------------------------------------------------
# ##@knitr err34
# err34 <- all_JAC_form %>% select(parish, holding, survtype,  all_of(c(owned_croft, area_own)), err34)  
# err34 <-err34 %>%  mutate(error=ifelse(err34== 1 & !is.na(err34), "Error", "No Error"))%>% filter(error == "Error")
# 
# ##----err 35----------------------------------------------------------------------------------------------
# ##@knitr err35
# err35 <- all_JAC_form %>% select(parish, holding, survtype,  all_of((owned_croft)), err35)  
# err35 <-err35 %>%  mutate(error=ifelse(err35== 1 & !is.na(err35), "Error", "No Error"))%>% filter(error == "Error")

##----err 36----------------------------------------------------------------------------------------------
##@knitr err36
err36 <- all_JAC_form %>% select(parish, holding, survtype,  all_of((rented_croft)), err36)  
err36 <-err36 %>%  mutate(error=ifelse(err36== 1 & !is.na(err36), "Error", "No Error"))%>% filter(error == "Error")

##----err 39----------------------------------------------------------------------------------------------
##@knitr err39
err39 <- check_labour %>% select(parish, holding, survtype, brn, land_data, saf_data, sumfruit, sumfruit_sum, slr_fruit, slr_fruit_sum, lab_returned, lab_returned_sum, err39)
err39 <-err39 %>%  mutate(error=ifelse(err39== 1 & !is.na(err39), "Error", "No Error"))%>% filter(error == "Error")


##----err 40----------------------------------------------------------------------------------------------
##@knitr err40
err40 <- check_labour %>% select(parish, holding, survtype, brn, land_data, saf_data, sumfruit, sumfruit_sum, slr_fruit, slr_fruit_sum, lab_returned, lab_returned_sum, err40)
err40 <-err40 %>%  mutate(error=ifelse(err40== 1 & !is.na(err40), "Error", "No Error"))%>% filter(error == "Error")

##----err 41----------------------------------------------------------------------------------------------
##@knitr err41
err41 <- check_labour %>% select(parish, holding, survtype, brn, land_data, saf_data, sumfruit, sumfruit_sum, slr_fruit, slr_fruit_sum, lab_returned, lab_returned_sum, err41)
err41 <-err41 %>%  mutate(error=ifelse(err41== 1 & !is.na(err41), "Error", "No Error"))%>% filter(error == "Error")

##----err 42----------------------------------------------------------------------------------------------
##@knitr err42
err42 <- check_labour %>% select(parish, holding, survtype, brn, land_data, saf_data, sumfruit, sumfruit_sum, slr_fruit, slr_fruit_sum, lab_returned, lab_returned_sum, err42)
err42 <-err42 %>%  mutate(error=ifelse(err42== 1 & !is.na(err42), "Error", "No Error"))%>% filter(error == "Error")



##----err-summary----------------------------------------------------------------------------------------------
##@knitr err-summary

JAC_validation_error_summary 


























