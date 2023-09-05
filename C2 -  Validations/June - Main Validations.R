

#JAC Validations for Census form data (C2, C3, C4 IN SAS)


# Clear environment prior
rm(list = ls())

library(tidyverse)
library(RtoSQLServer)
library(writexl)
library(janitor)



#yr = this year 20xx
yr <- 2023
yr1 <- yr - 1
yr2 <-  yr - 2
yr3 <-  yr - 3
yr_list <- c(yr, yr2, yr3)


#Functions -----------------------------------------------------------


#function to remove columns where total error = 0
remove_zero <- function(x) {
  select(x, where( ~ any(. != 0)))
}





# Before import -----------------------------------------------------------

# ADM schema for export
server <- "s0196a\\ADM"

database <- "RuralAndEnvironmentalScienceFarmingStatistics"
schema <- "juneagriculturalsurvey2023alpha"

#item_lists----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#source("item_numbers.R") - can't use source when using markdown
#manual copy paste from "item_numbers.R" file - 23rd  May 2023
##Individual item numbers JAC 2023

#NB new item numbers for 2023 have 5 digits


#Section 1---------------------------------------------------------------------
#Area you own
area_own <- "item11"

#RP&S area owned
rps_own <- "rps_totowned_june"

#Area of land you rent in on a full tenancy at this location
area_rent <- "item20026"
#area_rent_21 <- "item7"
rps_rent <- "rps_totrented_june"

#Total area
total_area <- "item12"

#RP&S total area
rps_total_area <-  "rps_totarea_june"

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
triticale <- "item15"
linseed <- "item21"

#Grains- SAF only
rye <- "item3156"
mixed_grain <- "item22"

#protein crops - SAF only
beans_combining <- "item27"
peas_combining <- "item28"
lupins <- "item2034"

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
aromatic_plants <-  "item2319"

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
glass_other_fruit_solid <- "item2708"
glass_veg_solid <- "item2709"
glass_bed_pot_plant_solid <- "item2710"
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
#glass_tom_tot <- "item87"
glass_strawb_tot <- "item2556"
glass_rasp_tot <- "item2557"
#glass_black_tot <- "item6000"
glass_blue_tot <- "item2836"
#glass_other_fruit_tot <- "item2036"
#glass_veg_tot <- "item2037"
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

# #----------items used up to 2021
# #Other pigs 110kg and over
# other_110_pig <- "item152"
# #Other pigs 80-110kg
# other_80_110_pig <- "item153"
# #Other pigs 50-80kg
# other_50_80_pig <- "item154"
# #Other pigs 20-50kg
# other_20_50_pig <- "item155"
# #Other pigs under 20kg
# other_lt20_pig <- "item156"

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

# #-----------------items used up to 2021
# #Horses Used in Agriculture
# agri_horses <- "item95"
# #All Other Horses and Ponies
# other_horse_pony <- "item96"
# #All Other Goats and kids
# other_goats_kids <- "item98"
# #Female Goats which have kidded this year or will kid this year
# female_goat_kids <- "item1712"
# #Other Female Goats which have been used for breeding in previous years
# other_female_goat <- "item1713"

#Section 8 ---------------------------------------------------------------------
#Legal Responsibility
#Do not return your details on more than one location. If you have already returned your details on another 1 June 2023 census form, please tick this box and go on to Section 10
other_legal_return <- "item2727"
#Who has legal and financial responsibility for this location?
legal_fin_resp <- "item2980"
#Section 9 ---------------------------------------------------------------------
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

#Occupier 2 details - Occupier 2 – can include formalised relationships (married couples and civil partnerships), cohabiting couples and close family relationships (father, mother, daughter, son and sibling)
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

#-----------------items used up to 2021 (spouse = occupier 2 from 2023, item numbers remain the same)
#Do not return your details on more than one location form.If you have already returned your details on another 2018 census form, please tick this box and go on to Section 9
#other_legal_return_21 <- "item2726"
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
non_crofting_parishes <-
  c(0:136,
    176:227,
    231,
    242:273,
    290:432,
    466:585,
    635:735,
    769:827,
    841:868)


#Description of errors----------------------------------------------------------------------------------------------------------------------------------------------------
# err1_desc <- "err1_Total area does not equal total area owned plus total area rented (by 3 or more hectares)"
# err10_desc <- "err10_Sum of itemised flowers bulbs and nursery stock in the open not equal to summary total"
# err11_desc <- "err11_Total glasshouse open soil area does not equal the sum of the breakdown of areas OR Total glasshouse solid floor area does not equal the sum of the breakdown of areas"
# err12_desc <- "err12_Glasshouse totals non zero but sum of individual items less than or equal to zero"
# err13_desc <- "err13_Total number of pigs does not equal the sum of the breakdown values"
# err14_desc <- "err14_Pigs not given as whole numbers"
# err15_desc <- "err15_Total number of sheep does not equal the sum of the breakdown values"
# err16_desc <- "err16_Sheep not given as whole numbers"
# err17_desc <- "err17_Total number of poultry does not equal the sum of the breakdown values"
# err18_desc <- "err18_Poultry not given as whole numbers"
# err19_desc <- "err19_Egg producing fowl = 0, cocks greater than 9"
# err2_desc <- "err2_Total seasonal rents is equal to area rented"
# err20_desc <- "err20_Not all deer given as whole numbers"
# err21_desc <- "err21_Not all camelids given as whole numbers"
# err22_desc <- "err22_Not all beehives given as whole numbers"
# err23_desc <- "err23_Not all other livestock given as whole numbers"
# err24_desc <- "err24_Number of other livestock entered but type not specified"
# err25_desc <- "err25_Not all horses given as whole numbers"
# err26_desc <- "err26_Not all goats given as whole numbers"
# err27_desc <- "err27_Not all horses given as whole numbers"
# err28_desc <- "err28_Not all goats given as whole numbers"
# err29_desc <- "err29_Other legal return box is ticked (labour details provided on another form) and Occupier 1 or occupier 2 work hours provided"
# err3_desc <- "err3_Seasonal let items are not equal to seasonal lets total"
# err30_desc <- "err30_Occupier 1: more than one full time / part time option selected"
# err31_desc <- "err31_Occupier 2: more than one full time / part time option selected"
# err32_desc <- "err32_Sum of other labour numbers does not equal Total"
# err33_desc <- "err33_Not all labour given as whole numbers"
# err34_desc <- "err34_Owned croft area is larger than area owned"
# err35_desc <- "err35_Owned croft area >0 and  is in non-crofting parish"
# err36_desc <- "err36_Rented croft area >0 and  is in non-crofting parish"
# err37_desc <- "err37_Total area less than margin of error - no area entered but area owned/rented non-zero"
# err38_desc <- "err38_Total area less than margin of error - no total area, no area owned and no area rented entered"
# err39_desc <- "err39_Migrant labour check: Soft fruit labour is too low-check whether missed other labour e.g. migrants "
# err4_desc <- "err4_Area of other crop entered but crop not specified (Area > 100ha) "
# err40_desc <- "err40_Migrant labour check: Soft fruit labour is too high- check for punch errors or misunderstanding of requirement of people/days in labour questions"
# err41_desc <- "err41_Migrant labour check: Soft fruit labour is too low- check whether missed other labour e.g. migrants"
# err42_desc <- "err42_Migrant labour check: Soft fruit labour is too high- check for punch errors or misunderstanding of requirement of people/days in labour questions"
# err43_desc <- "err43_Land seasonally rented out greater than total area"
# err44_desc <- "err44_Other crop specified but area not entered"
# err45_desc <- "err45_Total land use area does not equal the sum of the outdoor areas and glasshouses (3 ha margin)"
# err46_desc <- "err46_Total at end of land use section does not equal total area in section 1 (3 ha margin)"
# err47_desc <- "err47_Other livestock type specified but number not entered"
# err48_desc <- "err48_Occupier gender - both male and female selected"
# err49_desc <- "err49_Occupier 1 gender selected but none of the full time / part time options"
# err5_desc <- "err5_Total area of crops and grassland, rough grazing, woodland, and other land differs from total land by more than 3ha."
# err50_desc <- "err50_Occupier 1 gender selected but none of the full time / part time options"
# err51_desc <- "err51_Year of birth for Occupier 1 (and Occupier 2 if applicable) outside range (i.e. 1923 - 2008)"
# err52_desc <- "err52_Labour details NOT provided on another form this year & no Occupier 1 work hours provided"
# err53_desc <- "err53_Legal and financial responsbility not indicated for Occupier 2 (when any Occupier 2 details have been provided for Occupier 2)"
# err54_desc <- "err54_Legal and financial responsbility indicated for Occupier 2 but no working time answered"
# err55_desc <- "err55_Occupier 2 details selected but not Occupier 1 details"
# err56_desc <- "err56_Total area rented not equal to the current RP&S value (by 3 or more hectares)"
# err57_desc <- "err57_Total area rented not equal to the current RP&S value (by 30 or more hectares)"
# err58_desc <- "err58_Total area owned not equal to the current RP&S value (by 3 or more hectares)"
# err59_desc <- "err59_Total area owned not equal to the current RP&S value (by 30 or more hectares)"
# err6_desc <- "err6_Sum of itemised veg for human consumption not equal to all veg (SAF)"
# err7_desc <- "err7_All veg from SAF not equal to all veg (census form)"
# err8_desc <- "err8_Sum of itemised soft fruit not equal to all soft fruit (SAF)"
# err9_desc <- "err9_All soft fruit from SAF not equal to all soft fruit (census form)"
# err60_desc <- "err60_Legal responsbility box is not ticked (item2727) and no legal responsibility details (item2980) are given"
# err61_desc <- "err61_Sum of total area rented and total area owned not equal to sum of RP&S values (by 30 or more hectares)"


# Import ------------------------------------------------------------------

#combined dataset (SAF and JAC data)


combined_JAC <- read_table_from_db(
  server = server,
  database = database,
  schema = schema,
  table_name = "combined_data_2023"
)


combined_JAC <- combined_JAC %>% group_by(parish, holding)

#croft dataset

croft <- read_table_from_db(
  server = server,
  database = database,
  schema = schema,
  table_name = "crofts_A_2023"
)

croft <- croft %>% group_by(parish, holding)


# Combine croft and combined dataset------------------------------------------------------------------

#filter for only form returns
all_JAC_form <- combined_JAC %>% group_by(parish, holding) %>%
  filter(survtype == "Non-SAF")
croft <- croft %>% mutate(croft = 1)

#form returns and croft dataset
all_JAC_form <-
  left_join(all_JAC_form, croft, by = c("parish", "holding")) %>%
  mutate(croft = case_when(croft == 1 ~ as.numeric(croft),
                           TRUE ~ 0))

#Validations---------------------------------------------------------------------------------------

#NOTE: a value of 1 in the "err(x)" column = holding data has failed logic of validation test. A value of 0 means data for that holding are fine.

# Section 1 Area of holding--------------------


#Total area not equal to area rent + area own (3 ha margin)
#err1
all_JAC_form$err1 <-
  ifelse(abs(
    round(all_JAC_form[total_area], digits = 2) - round(all_JAC_form[area_own] +
                                                          all_JAC_form[area_rent], digits = 2)
  ) >= 3,  1, 0)
all_JAC_form$err1_diff <-  ifelse(all_JAC_form$err1 == 1,
                                  abs(rowSums((all_JAC_form[area_own] + all_JAC_form[area_rent]) - all_JAC_form[total_area]
                                  )), 0)

#Total area is zero but area own or area rented >0
#err37
all_JAC_form$err37 <-
  ifelse((all_JAC_form[total_area] == 0 |
            is.na(all_JAC_form[total_area])) &
           (all_JAC_form[area_own] | all_JAC_form[area_rent] > 0),
         1,
         0)


#Total area is zero and area owned and area rented =0
#err38
all_JAC_form$err38 <-
  ifelse((all_JAC_form[total_area] == 0 |
            is.na(all_JAC_form[total_area])) &
           (all_JAC_form[area_own] == 0 |
              is.na(all_JAC_form[area_own])) &
           all_JAC_form[area_rent] == 0 |
           is.na(all_JAC_form[area_rent]),
         1,
         0
  )


#Area rented greater/less than RP&S value by 30ha or more
#err57
all_JAC_form$err57 <-
  ifelse(abs(
    round(all_JAC_form[area_rent], digits = 2) - round(all_JAC_form[rps_rent], digits = 2)
  ) >= 30, 1, 0)
all_JAC_form$err57_diff <-  ifelse(all_JAC_form$err57 == 1,
                                   rowSums(abs(all_JAC_form[area_rent] - all_JAC_form[rps_rent])), 0)


#Area owned greater/less than RP&S value by by 30ha or more
#err59
all_JAC_form$err59 <-
  ifelse(abs(
    round(all_JAC_form[area_own], digits = 2) - round(all_JAC_form[rps_own], digits = 2)
  ) >= 30,  1, 0)
all_JAC_form$err59_diff <-  ifelse(all_JAC_form$err59 == 1,
                                   rowSums(abs(all_JAC_form[area_own] - all_JAC_form[rps_own])), 0)

#Sum of area owned and rented is not equal to sum of RP&S values by 30 or more hectares
#err61
total14 <- c(area_own, area_rent)
total14rps <- c(rps_own, rps_rent)
all_JAC_form$total14 <- rowSums(all_JAC_form[total14], na.rm = TRUE)
all_JAC_form$total14rps <-
  rowSums(all_JAC_form[total14rps], na.rm = TRUE)

all_JAC_form$err61 <-
  ifelse(abs(
    round(all_JAC_form$total14, digits = 2) - round(all_JAC_form$total14rps, digits = 2)
  ) >= 30,  1, 0)

all_JAC_form$err61_diff <-  ifelse(all_JAC_form$err61 == 1,
                                   abs(
                                     round(all_JAC_form$total14 - all_JAC_form$total14rps, digits = 2)
                                   ), 0)


# Section 2 Seasonal rents--------------------


#Total seasonal rent area = area rent in section 1
#err2
all_JAC_form$err2 <-
  ifelse(
    round(all_JAC_form[seas_total_area_in], digits = 2) == round(all_JAC_form[area_rent], digits = 2)
    &
      (all_JAC_form[seas_total_area_in] &
         all_JAC_form[area_rent] > 0) &
      all_JAC_form$land_data == "ags",
    1,
    0
  )


#new seasonal rent + same location seasonal rent = total seasonal rent area
#err3
all_JAC_form$err3 <-
  ifelse(!is.na(all_JAC_form[seas_total_area_in]),
         ifelse((
           round(all_JAC_form[seas_rent_diff_in] + all_JAC_form[seas_rent_same_in], digits = 2) != round(all_JAC_form[seas_total_area_in], digits = 2)
         )
         &
           all_JAC_form$land_data != "both",  1, 0),
         0)

all_JAC_form$err3_diff <- ifelse(all_JAC_form$err3 == 1,
                                 rowSums(all_JAC_form[seas_total_area_in] - (all_JAC_form[seas_rent_diff_in] + all_JAC_form[seas_rent_same_in])), 0)

#err43 Land seasonally rented out greater than total area
#err43
all_JAC_form$err43 <-
  ifelse(round(all_JAC_form[seas_area_rent_out], digits = 2) - round(all_JAC_form[total_area], digits = 2) >=
           3,
         1,
         0)
all_JAC_form$err43_diff <- ifelse(all_JAC_form$err43 == 1,
                                  rowSums(all_JAC_form[seas_area_rent_out] - all_JAC_form[total_area]),
                                  0)


# Section 3 Land Use --------------------------

#Validate/QA SAF totals (adding itemised crops) vs form summary totals?

#name of other crop given if area > 100 ha
#err4
all_JAC_form$err4 <-
  ifelse(all_JAC_form[other_crops] > 100 &
           (is.na(all_JAC_form[other_crops_text]) |
              all_JAC_form[other_crops_text] == ""),  1, 0)

#Other crop specified but area not given
#err44
all_JAC_form$err44 <-
  ifelse(!is.na(all_JAC_form[other_crops_text])  &
           all_JAC_form[other_crops] <= 0,  1, 0)

#Section 3 Grassland-------------------------------------------------------------------------------------------------------------------------------------------------
#total area of crops and grassland, rough grazing, woodland and other land is not equal to total land



total7 <- c(total_crops_grass, rough_graze, woodland, other_land)
all_JAC_form$total7 <- rowSums(all_JAC_form[total7], na.rm = TRUE)

all_JAC_form$err5 <-
  ifelse(all_JAC_form$land_data == "ags", ifelse(abs(
    round(all_JAC_form[total_land], digits = 2) - round(all_JAC_form$total7, digits = 2)
  ) >= 3, 1, 0), 0)
all_JAC_form$err5_diff <- ifelse(all_JAC_form$err5 == 1,
                                 abs(rowSums(all_JAC_form[total_land] - all_JAC_form$total7)), 0)



#Section 3 Vegetables----------------------------------------------------------------------------------------------------------------------------------------------
total2 <-
  c(
    peas,
    beans,
    leeks,
    turn_swed,
    cab_sav,
    brussel,
    calabrese,
    cauli_broc,
    carrots,
    lettuce,
    rhubarb,
    other_veg
  )
all_JAC_form$total2 <- rowSums(all_JAC_form[total2], na.rm = TRUE)


# # sum of itemised veg for human consumption not equal to all veg (SAF)
# all_JAC_form$err6 <-  ifelse(round(all_JAC_form[all_veg_saf], digits = 2) != round(all_JAC_form$total2, digits = 2), 1, 0)
# all_JAC_form$err6_diff <- round(rowSums(all_JAC_form[all_veg_saf]- all_JAC_form$total2), digits=2)
# all_JAC_form$err6_diff <- abs(all_JAC_form$err6_diff)

#err7 = err19 in SAS
#all veg from SAF not equal to all veg (census form)
all_JAC_form$err7 <-
  ifelse(!is.na(all_JAC_form[all_veg_saf]) &
           !is.na(all_JAC_form[all_veg_open]),
         ifelse(
           round(all_JAC_form[all_veg_saf], digits = 2) !=
             round(all_JAC_form[all_veg_open], digits = 2),
           1,
           0
         ),
         0)
all_JAC_form$err7_diff <-
  ifelse(all_JAC_form$err7 == 1, abs(round(
    rowSums(all_JAC_form[all_veg_saf] - all_JAC_form[all_veg_open]), digits = 2
  )), 0)


#Soft fruit
total3 <-
  c(strawberries,
    raspberries,
    blackcurrants,
    blueberries,
    mixed_soft_fruit)
all_JAC_form$total3 <- rowSums(all_JAC_form[total3], na.rm = TRUE)

#err8
# sum of itemised soft fruit not equal to all soft fruit (SAF)
all_JAC_form$err8 <-
  ifelse(round(all_JAC_form[all_fruit_saf], digits = 2) != round(all_JAC_form$total3, digits = 2),
         1,
         0)
all_JAC_form$err8_diff <-
  ifelse(all_JAC_form$err8 == 1, abs(rowSums(all_JAC_form[all_fruit_saf] - all_JAC_form$total3)), 0)


#err9
#all soft fruit from SAF not equal to all soft fruit (census form)
all_JAC_form$err9 <-
  ifelse(!is.na(all_JAC_form[soft_fruits]) &
           !is.na(all_JAC_form[all_fruit_saf]),
         ifelse(
           round(all_JAC_form[soft_fruits], digits = 2) !=
             round(all_JAC_form[all_fruit_saf], digits = 2),
           1,
           0
         ),
         0)
all_JAC_form$err9_diff <- ifelse(all_JAC_form$err9 == 1,
                                 abs(rowSums(all_JAC_form[soft_fruits] - all_JAC_form[all_fruit_saf])), 0)


#Flowers, bulbs and nursery stock grown in the open

total4 <-
  c(
    all_flow_bulb_SAF,
    bed_pot_plant,
    fruit_stocks,
    roses_stocks,
    orn_trees,
    shrubs,
    other_nurs
  )
all_JAC_form$total4 <- rowSums(all_JAC_form[total4], na.rm = TRUE)

#err10 =
#sum of itemised flowers bulbs and nursery stock in the open not equal to summary total
all_JAC_form$err10 <-
  ifelse(!is.na(all_JAC_form[all_flow_bulb]) &
           !is.na(all_JAC_form$total4),
         ifelse(
           round(all_JAC_form[all_flow_bulb], digits = 2) !=
             round(all_JAC_form$total4, digits = 2),
           1,
           0
         ),
         0)
all_JAC_form$err10_diff <- ifelse(all_JAC_form$err10 == 1,
                                  abs(rowSums(all_JAC_form[all_flow_bulb] - all_JAC_form$total4)), 0)




#glasshouses

total5b <- c(all_glass_open, glass_unused_open)
total6b <- c(all_glass_solid, glass_unused_solid)
all_JAC_form$total5b <- rowSums(all_JAC_form[total5b], na.rm = TRUE)
all_JAC_form$total6b <- rowSums(all_JAC_form[total6b], na.rm = TRUE)

all_JAC_form$err11 <-
  ifelse((
    round(all_JAC_form$total5b, digits = 2) != round(all_JAC_form[tot_open_plastic], digits = 2)
  ) |
    (
      round(all_JAC_form$total6b, digits = 2) != round(all_JAC_form[tot_solid_glass], digits = 2)
    ),  1, 0)


all_JAC_form$err11open_diff <-
  ifelse(all_JAC_form$err11 == 1, abs(rowSums(all_JAC_form[tot_open_plastic] - all_JAC_form$total5b)), 0)

all_JAC_form$err11solid_diff <-
  ifelse(all_JAC_form$err11 == 1, abs(rowSums(all_JAC_form[tot_solid_glass] - all_JAC_form$total6b)), 0)


all_JAC_form$err12 <-
  ifelse(
    !is.na(all_JAC_form[tot_open_plastic]) &
      !is.na(all_JAC_form[tot_solid_glass]) &
      !is.na(all_JAC_form$total5b) & !is.na(all_JAC_form$total6b),
    ifelse(
      all_JAC_form[tot_open_plastic] + all_JAC_form[tot_solid_glass] > 0 &
        (all_JAC_form$total5b + all_JAC_form$total6b) <= 0,
      1,
      0
    ),
    0
  )

all_JAC_form$err12_diff <-
  ifelse(all_JAC_form$err12 == 1, abs(rowSums((all_JAC_form[tot_open_plastic] + all_JAC_form[tot_solid_glass]) - (all_JAC_form$total5b + all_JAC_form$total6b)
  )), 0)


#Glasshouse  total not equal to sum of open soil + solid floor
#To do?


#Total land use area does not equal the sum of the outdoor areas and glasshouses (3ha margin of error)

# total13 <- c(wheat, winter_barley, spring_barley, winter_oats, spring_oats, winter_rape, spring_rape, triticale, mixed_grain, rye,
#              linseed, peas_combining, beans_combining, seed_potatoes, ware_potatoes, lupins, turn_swed_sf, kale_cab_sf, rape_sf,
#              maize, fbeet_sf, other_sf, peas, beans, leeks, cab_sav, brussel, calabrese, cauli_broc, turn_swed, carrots, lettuce, rhubarb,
#              other_veg, orchard_fruit, strawberries, raspberries, blackcurrants, blueberries, mixed_soft_fruit, bed_pot_plant, all_flow_bulb_SAF,
#              fruit_stocks, roses_stocks, shrubs, orn_trees, aromatic_plants, other_nurs, tot_open_plastic, tot_solid_glass, other_crops, fland_lte5, fland_gt5, grass_lt5, grass_gte5,
#              rough_graze, woodland, other_land)

#for 2023 onwards- census form using summary values w/out SAF returns
total13b <-
  c(
    wheat,
    all_barley,
    all_oats,
    all_rape,
    seed_potatoes,
    ware_potatoes,
    all_stockfeed,
    all_veg_open,
    all_fruit,
    all_flow_bulb,
    tot_open_plastic,
    tot_solid_glass,
    other_crops,
    fland_lte5,
    fland_gt5,
    grass_lt5,
    grass_gte5,
    rough_graze,
    woodland,
    other_land
  )


#all_JAC_form$total13 <- rowSums(all_JAC_form %>% ungroup %>%  select(any_of(total13)), na.rm =TRUE)
all_JAC_form$total13 <-
  rowSums(all_JAC_form %>% ungroup %>%  select(any_of(total13b)), na.rm =
            TRUE)

#(may need to do ifelse survtype == Non-SAF? (or SAF))
all_JAC_form$err45 <-
  ifelse(!is.na(all_JAC_form[total_land]),
         ifelse(
           all_JAC_form$land_data == "ags" &
             abs(all_JAC_form[total_land] - all_JAC_form$total13) >= 3,
           1,
           0
         ),
         0)
all_JAC_form$err45_diff <-
  ifelse(all_JAC_form$err45 == 1, abs(rowSums(all_JAC_form[total_land] - all_JAC_form$total13)),  0)


#Total at end of land use section equal to total area

all_JAC_form$err46 <-
  ifelse(
    !is.na(all_JAC_form[total_land]) &
      !is.na(all_JAC_form[total_area]),
    ifelse(
      all_JAC_form$land_data == "ags" &
        abs(all_JAC_form[total_land] - all_JAC_form[total_area]) >= 3,
      1,
      0
    ),
    0
  )
all_JAC_form$err46_diff <-
  ifelse(all_JAC_form$err46 == 1, abs(rowSums(all_JAC_form[total_land] - all_JAC_form[total_area])), 0)


# Section 4 Pigs--------------------

#2023 version
total10 <-
  c(
    sows_pig,
    gilts_pig,
    other_sows_pig,
    barren_sows_pig,
    gilts_50_pig,
    boars_pig,
    fat_pig,
    wean_pig,
    piglet
  )

all_JAC_form$total10 <- rowSums(all_JAC_form[total10], na.rm = TRUE)

#Total pigs is not equal to sum of individual pig items
all_JAC_form$err13 <-
  ifelse(round(all_JAC_form$total10, digits = 1) != round(all_JAC_form[total_pig], digits = 1),
         1,
         0)
all_JAC_form$err13_diff <-
  ifelse(all_JAC_form$err13 == 1,  abs(rowSums(all_JAC_form[total_pig] - all_JAC_form$total10)), 0)


#Pigs not given as whole numbers


#new pig item numbers for 2023 - comment out above and uncomment below
all_JAC_form$err14 <-
  ifelse(
    round(all_JAC_form[sows_pig], digits = 0) != all_JAC_form[sows_pig] |
      round(all_JAC_form[gilts_pig], digits = 0) != all_JAC_form[gilts_pig] |
      round(all_JAC_form[other_sows_pig], digits = 0) != all_JAC_form[other_sows_pig] |
      round(all_JAC_form[barren_sows_pig], digits = 0) != all_JAC_form[barren_sows_pig] |
      round(all_JAC_form[gilts_50_pig], digits = 0) != all_JAC_form[gilts_50_pig] |
      round(all_JAC_form[boars_pig], digits = 0) != all_JAC_form[boars_pig] |
      round(all_JAC_form[fat_pig], digits = 0) != all_JAC_form[fat_pig] |
      round(all_JAC_form[wean_pig], digits = 0) != all_JAC_form[wean_pig] |
      round(all_JAC_form[piglet], digits = 0) != all_JAC_form[piglet],
    1,
    0
  )

#Section 5 Sheep--------------------

total9 <-
  c(ewes, rams, breed_other_sheep, no_breed_other_sheep, lambs)
all_JAC_form$total9 <- rowSums(all_JAC_form[total9], na.rm = TRUE)

#Total sheep not equal to sum of individual sheep items
#err15 = err30 in SAS
all_JAC_form$err15 <-
  ifelse(round(all_JAC_form$total9, digits = 1) != round(all_JAC_form[total_sheep], digits = 1),
         1,
         0)
all_JAC_form$err15_diff <-
  ifelse(all_JAC_form$err15 == 1, abs(rowSums(all_JAC_form[total_sheep] - all_JAC_form$total9)), 0)


#Sheep not given as whole numbers
#err16 = err31 in SAS
all_JAC_form$err16 <-
  ifelse(
    round(all_JAC_form[ewes], digits = 0) != all_JAC_form[ewes] |
      round(all_JAC_form[rams], digits = 0) != all_JAC_form[rams] |
      round(all_JAC_form[breed_other_sheep], digits = 0) != all_JAC_form[breed_other_sheep] |
      round(all_JAC_form[no_breed_other_sheep], digits = 0) != all_JAC_form[no_breed_other_sheep] |
      round(all_JAC_form[lambs], digits = 0) != all_JAC_form[lambs] |
      round(all_JAC_form[total_sheep], digits = 0) != all_JAC_form[total_sheep],
    1,
    0
  )

#Section 6 Poultry--------------------

total11 <-
  c(
    first_hens,
    moulted_hens,
    pullets,
    layer_chicks,
    table_chicks,
    cocks,
    broilers,
    turkeys,
    ducks,
    geese,
    other_poultry
  )
all_JAC_form[total11] <-
  all_JAC_form[total11] %>% mutate(across(where(is.numeric), ~  ifelse(is.na(.), 0, .)))
all_JAC_form$total11 <- rowSums(all_JAC_form[total11], na.rm = TRUE)

#Total poultry not equal to sum of individual poultry items
#err17 = err27 in SAS = match (1)
all_JAC_form$err17 <-
  ifelse(round(all_JAC_form$total11, digits = 1) != round(all_JAC_form[total_poultry], digits = 1),
         1,
         0)
all_JAC_form$err17_diff <-
  ifelse(all_JAC_form$err17 == 1,  abs(rowSums(all_JAC_form[total_poultry] - all_JAC_form$total11)), 0)

#Poultry not given as whole numbers
#err18 = err28 in SAS
all_JAC_form$err18 <-
  ifelse(
    round(all_JAC_form[first_hens], digits = 0) != all_JAC_form[first_hens] |
      round(all_JAC_form[moulted_hens], digits = 0) != all_JAC_form[moulted_hens] |
      round(all_JAC_form[pullets], digits = 0) != all_JAC_form[pullets] |
      round(all_JAC_form[layer_chicks], digits = 0) != all_JAC_form[layer_chicks] |
      round(all_JAC_form[table_chicks], digits = 0) != all_JAC_form[table_chicks] |
      round(all_JAC_form[cocks], digits = 0) != all_JAC_form[cocks] |
      round(all_JAC_form[broilers], digits = 0) != all_JAC_form[broilers] |
      round(all_JAC_form[turkeys], digits = 0) != all_JAC_form[turkeys] |
      round(all_JAC_form[ducks], digits = 0) != all_JAC_form[ducks] |
      round(all_JAC_form[geese], digits = 0) != all_JAC_form[geese] |
      round(all_JAC_form[other_poultry], digits = 0) != all_JAC_form[other_poultry] |
      round(all_JAC_form[total_poultry], digits = 0) != all_JAC_form[total_poultry],
    1,
    0
  )

#Egg producing fowl = 0, cocks greater than 9
#err19 = err29 in SAS
all_JAC_form$err19 <-
  ifelse(round((
    all_JAC_form[first_hens] + all_JAC_form[moulted_hens] + all_JAC_form[pullets] + all_JAC_form[layer_chicks] +
      all_JAC_form[table_chicks] + all_JAC_form[broilers]
  ),
  digits = 1
  ) == 0 & round(all_JAC_form[cocks], digits = 1) > 9, 1, 0)


#Section 7 Miscellaneous Livestock--------------------

#Not all deer given as whole numbers
#err20 = err32 in SAS
all_JAC_form$err20 <-
  ifelse(round(all_JAC_form[deer], digits = 0) != all_JAC_form[deer], 1, 0)

#Not all camelids given as whole numbers
#err21 = err35 in SAS
all_JAC_form$err21 <-
  ifelse(
    round(all_JAC_form[alpacas], digits = 0) != all_JAC_form[alpacas] |
      round(all_JAC_form[llamas], digits = 0) != all_JAC_form[llamas] |
      round(all_JAC_form[other_camelids], digits = 0) != all_JAC_form[other_camelids],
    1,
    0
  )

#Not all beehives given as whole numbers
#err22 = err36 in SAS
all_JAC_form$err22 <-
  ifelse(round(all_JAC_form[hives], digits = 0) != all_JAC_form[hives], 1, 0)

#Not all other livestock given as whole numbers
#err23 = err37 in SAS
all_JAC_form$err23 <-
  ifelse(round(all_JAC_form[other_livestock], digits = 0) != all_JAC_form[other_livestock], 1, 0)

#Other livestock is non-zero but no name is given
#err24 = err38 in SAS
all_JAC_form$err24 <-
  ifelse(all_JAC_form[other_livestock] > 0 &
           (is.na(all_JAC_form[other_livestock_txt]) |
              all_JAC_form[other_livestock_txt] == ""),  1, 0)

#Other livestock specified but number not given. This gives some blank item186 as errors - needs to be fixed. 


# Create err47 first without NAs (otherwise following bit will)


all_JAC_form$err47 <-
  ifelse(!is.na(all_JAC_form[other_livestock_txt]) &
           all_JAC_form[other_livestock] <= 0,  1, 0)



all_JAC_form$err27 <-
  ifelse(
    round(all_JAC_form[donkeys], digits = 1) != all_JAC_form[donkeys] |
      round(all_JAC_form[horse_pony], digits = 1) != all_JAC_form[horse_pony],
    1,
    0
  )

#err28 = err34 in SAS
all_JAC_form$err28 <-
  ifelse(round(all_JAC_form[goats_kids], digits = 1) != all_JAC_form[goats_kids], 1, 0)


#Section 8 Legal Responsbility----------------------------------------------------------------------------------------------

#legal and financial responsibility not answered and no other legal return or no occupier details
all_JAC_form$err60 <-
  ifelse(
    all_JAC_form[legal_fin_resp] == 0 &
      (all_JAC_form[other_legal_return] == 0 |
      (
        all_JAC_form[occupier1_ft] + all_JAC_form[occupier1_pt_gthalf] +
          all_JAC_form[occupier1_pt_lthalf] + all_JAC_form[occupier1_no_work] + all_JAC_form[occupier2_ft] + all_JAC_form[occupier2_pt_gthalf] +
          all_JAC_form[occupier2_pt_lthalf] + all_JAC_form[occupier2_no_work]
      ) <= 0),
    1,
    0
  )


#'Other legal return box is ticked (labour details provided on another form) and Occupier 1 or occupier 2 work hours provided


#2023 onwards
all_JAC_form$err29 <-
  ifelse(
    all_JAC_form[other_legal_return] == 1 &
      (
        all_JAC_form[occupier1_ft] + all_JAC_form[occupier1_pt_gthalf] +
          all_JAC_form[occupier1_pt_lthalf] + all_JAC_form[occupier1_no_work] +
          all_JAC_form[occupier2_ft] + all_JAC_form[occupier2_pt_gthalf] +
          all_JAC_form[occupier2_pt_lthalf] + all_JAC_form[occupier2_no_work]
      ) > 0,
    1,
    0
  )


#Labour details NOT provided on another form this year & no Occupier 1 or occupier 2 work hours provided

#2023 onwards
all_JAC_form$err52 <-
  ifelse(
    all_JAC_form[other_legal_return] == 0 &
      (
        all_JAC_form[occupier1_ft] + all_JAC_form[occupier1_pt_gthalf] +
          all_JAC_form[occupier1_pt_lthalf] + all_JAC_form[occupier1_no_work] + all_JAC_form[occupier2_ft] + all_JAC_form[occupier2_pt_gthalf] +
          all_JAC_form[occupier2_pt_lthalf] + all_JAC_form[occupier2_no_work]
      ) <= 0,
    1,
    0
  )


#Legal and financial responsbility not indicated for Occupier 2 (when any Occupier 2 details have been provided for Occupier 2)
all_JAC_form$err53 <-
  ifelse((all_JAC_form[occupier2_legal_fin] == 0 |
            all_JAC_form[occupier2_legal_fin] == "" |
            is.na(all_JAC_form[occupier2_legal_fin])) &
           (all_JAC_form[occupier2_ft] + all_JAC_form[occupier2_pt_gthalf] +
              all_JAC_form[occupier2_pt_lthalf] + all_JAC_form[occupier2_no_work]) >
           0,
         1,
         0
  )


#Legal and financial responsbility indicated for Occupier 2 but no working time answered
all_JAC_form$err54 <-
  ifelse(
    all_JAC_form[occupier2_legal_fin] > 0 &
      (all_JAC_form[occupier2_ft] + all_JAC_form[occupier2_pt_gthalf] +
         all_JAC_form[occupier2_pt_lthalf] + all_JAC_form[occupier2_no_work]) <=
      0,
    1,
    0
  )


#Section 9 Occupiers----------------------------------------------------------------------------------------------------------------------------------------------------

#More than one working time option provided for occupier 1
#err30
all_JAC_form$err30 <-
  ifelse(
    all_JAC_form[occupier1_ft] >= 1 &
      all_JAC_form[occupier1_pt_gthalf] >= 1 |
      all_JAC_form[occupier1_ft] >= 1 &
      all_JAC_form[occupier1_pt_lthalf] >= 1 |
      all_JAC_form[occupier1_pt_gthalf] >= 1 &
      all_JAC_form[occupier1_pt_lthalf] >= 1 |
      all_JAC_form[occupier1_ft] >= 1 &
      all_JAC_form[occupier1_no_work] >= 1 |
      all_JAC_form[occupier1_pt_gthalf] >= 1 &
      all_JAC_form[occupier1_no_work] >= 1 |
      all_JAC_form[occupier1_pt_lthalf] >= 1 &
      all_JAC_form[occupier1_no_work] >= 1,
    1,
    0
  )

#More than one working time option provided for occupier 2
#err31 = err41 in SAS = no match (28 vs 23 in SAS)
all_JAC_form$err31 <-
  ifelse(
    all_JAC_form[occupier2_ft] >= 1 &
      all_JAC_form[occupier2_pt_gthalf] >= 1 |
      all_JAC_form[occupier2_ft] >= 1 &
      all_JAC_form[occupier2_pt_lthalf] >= 1 |
      all_JAC_form[occupier2_pt_gthalf] >= 1 &
      all_JAC_form[occupier2_pt_lthalf] >= 1 |
      all_JAC_form[occupier2_ft] >= 1 &
      all_JAC_form[occupier2_no_work] >= 1 |
      all_JAC_form[occupier2_pt_gthalf] >= 1 &
      all_JAC_form[occupier2_no_work] >= 1 |
      all_JAC_form[occupier2_pt_lthalf] >= 1 &
      all_JAC_form[occupier2_no_work] >= 1,
    1,
    0
  )


#both male and female selected for occupier 1 or 2
all_JAC_form$err48 <-
  ifelse((all_JAC_form[occupier1_male] > 0 &
            all_JAC_form[occupier1_female] > 0) |
           (all_JAC_form[occupier2_male] > 0 &
              all_JAC_form[occupier2_female] > 0),
         1,
         0)

#gender selected but no working hours given
#occupier1
all_JAC_form$err49 <-
  ifelse((all_JAC_form[occupier1_male] > 0 |
            all_JAC_form[occupier1_female] > 0)  &
           (all_JAC_form[occupier1_ft] + all_JAC_form[occupier1_pt_gthalf] +
              all_JAC_form[occupier1_pt_lthalf] + all_JAC_form[occupier1_no_work]) <=
           0,
         1,
         0
  )
#occupier2
all_JAC_form$err50 <-
  ifelse((all_JAC_form[occupier2_male] > 0 |
            all_JAC_form[occupier2_female] > 0)  &
           (all_JAC_form[occupier2_ft] + all_JAC_form[occupier2_pt_gthalf] +
              all_JAC_form[occupier2_pt_lthalf] + all_JAC_form[occupier2_no_work]) <=
           0,
         1,
         0
  )

#2023 check if valid Year of birth for occupier 1 and 2
all_JAC_form$err51 <-
  ifelse((all_JAC_form[occupier1_year] != 0 &
            (all_JAC_form[occupier1_year] < 1923 |
            all_JAC_form[occupier1_year] > 2008)) |
           (all_JAC_form[occupier2_year] != 0 &
              (all_JAC_form[occupier2_year] < 1923 |
              all_JAC_form[occupier2_year] > 2008)),
         1,
         0
  )


#Occupier 2 details given but not occupier 1
all_JAC_form$err55 <-
  ifelse((
    all_JAC_form[occupier2_ft] + all_JAC_form[occupier2_pt_gthalf] + all_JAC_form[occupier2_pt_lthalf] + all_JAC_form[occupier2_no_work] + all_JAC_form[occupier2_female] +
      all_JAC_form[occupier2_male] > 0
  ) &
    (
      all_JAC_form[occupier1_ft] + all_JAC_form[occupier1_pt_gthalf] + all_JAC_form[occupier1_pt_lthalf] + all_JAC_form[occupier1_no_work] +
        all_JAC_form[occupier1_female] + all_JAC_form[occupier1_male] <=
        0
    ),
  1,
  0
  )


#Section 10 Other labour on the location------------------------------------------------------------------------------------------------------------------------
total12 <-
  c(
    labour_ft_m_bp,
    labour_ft_m_fam,
    labour_ft_m_hired,
    labour_ft_f_bp,
    labour_ft_f_fam,
    labour_ft_f_hired,
    labour_pt_m_bp,
    labour_pt_f_bp,
    labour_pt_m_hired,
    labour_pt_m_fam,
    labour_pt_f_hired,
    labour_pt_f_fam,
    labour_cas_m,
    labour_cas_f
  )

all_JAC_form$total12 <- rowSums(all_JAC_form[total12], na.rm = TRUE)
#total labour  not equal to sum of individual items
#err32 = err42 in SAS = match (4)
all_JAC_form$err32 <-
  ifelse(round(all_JAC_form[total_labour], digits = 1) != all_JAC_form$total12, 1, 0)
all_JAC_form$err32_diff <-
  ifelse(all_JAC_form$err32 == 1, rowSums(abs(all_JAC_form[total_labour] - all_JAC_form$total12)), 0)
all_JAC_form$err32_diff <- abs(all_JAC_form$err32_diff)

#Not all labour given as whole numbers
#err33 = err43 in SAS
all_JAC_form$err33 <-
  ifelse(
    round(all_JAC_form[labour_ft_m_bp], digits = 0) != all_JAC_form[labour_ft_m_bp] |
      round(all_JAC_form[labour_ft_m_hired], digits = 0) != all_JAC_form[labour_ft_m_hired] |
      round(all_JAC_form[labour_ft_m_fam], digits = 0) != all_JAC_form[labour_ft_m_fam] |
      round(all_JAC_form[labour_ft_f_bp], digits = 0) != all_JAC_form[labour_ft_f_bp] |
      round(all_JAC_form[labour_ft_f_hired], digits = 0) != all_JAC_form[labour_ft_f_hired] |
      round(all_JAC_form[labour_ft_f_fam], digits = 0) != all_JAC_form[labour_ft_f_fam] |
      round(all_JAC_form[labour_pt_m_bp], digits = 0) != all_JAC_form[labour_pt_m_bp] |
      round(all_JAC_form[labour_pt_m_hired], digits = 0) != all_JAC_form[labour_pt_m_hired] |
      round(all_JAC_form[labour_pt_m_fam], digits = 0) != all_JAC_form[labour_pt_m_fam] |
      round(all_JAC_form[labour_pt_f_bp], digits = 0) != all_JAC_form[labour_pt_f_bp] |
      round(all_JAC_form[labour_pt_f_hired], digits = 0) != all_JAC_form[labour_pt_f_hired] |
      round(all_JAC_form[labour_pt_f_fam], digits = 0) != all_JAC_form[labour_pt_f_fam] |
      round(all_JAC_form[labour_cas_m], digits = 0) != all_JAC_form[labour_cas_m] |
      round(all_JAC_form[labour_cas_f], digits = 0) != all_JAC_form[labour_cas_f] |
      round(all_JAC_form[total_labour], digits = 0) != all_JAC_form[total_labour],
    1,
    0
  )

#croft---------------------------------------------------------------------------------------------------------------------------------------------------------
#err34 = err44 in SAS
#Owned croft area is larger than area owned
#item2249 isn't in all_JAC_form?
# all_JAC_form$err34 <- ifelse(all_JAC_form[owned_croft]> all_JAC_form[area_own], 1, 0)
#
# #err35 = err3 in SAS
# #item2249 doesn't exist
# #Owned croft area >0 and  is in non-crofting parish
# all_JAC_form$err35 <- ifelse (all_JAC_form[owned_croft] > 0 & all_JAC_form$parish %in% non_crofting_parishes, 1, 0)
#
# #err36 = err4 in SAS
# #Rented croft area >0 and  is in non-crofting parish
# all_JAC_form$err36 <- ifelse (all_JAC_form[rented_croft] > 0 & all_JAC_form$parish %in% non_crofting_parishes, 1, 0)


#migrant worker suppression - need to write function

#Migrant worker check (C4)---------------------------------------------------------------------------------------

d15 <-
  c(#glass_tom_tot, (item87 no longer collected from form- SAF dataset only collects item2707 (protected crops tomatoes solid floor))
    glass_tom_solid,
    #glass_other_fruit_tot, not on form (open or solid) and not in SAF 2023
    #glass_veg_tot,  not on form (open or solid) and not in SAF 2023
    glass_strawb_tot,
    glass_rasp_tot,
    glass_blue_tot)
#glass_black_tot- item6000 cannot be made as blackcurrant grown on open soil item2861 not on SAF data or form just solid in SAF)

g01a <-
  c(orchard_fruit,
    raspberries,
    blackcurrants,
    blueberries,
    mixed_soft_fruit)
glass_other_fruit_open
fruit_holdings <- combined_JAC
fruit_holdings$d15 <- rowSums(fruit_holdings[d15], na.rm = TRUE)
fruit_holdings$g01a <- rowSums(fruit_holdings[g01a], na.rm = TRUE)
fruit_holdings$sumfruit <- fruit_holdings$d15 + fruit_holdings$g01a

fruit_holdings$slr_d15 <- 7000.0
fruit_holdings$slr_g01 <- 425.0
fruit_holdings$slr_fruit <-
  ((fruit_holdings$slr_d15 * fruit_holdings$d15) + (fruit_holdings$slr_g01 *
                                                      fruit_holdings$g01a)
  ) / 1900


fruit_holdings$ftocc1 <-
  ifelse(fruit_holdings[occupier1_ft] > 0, 1, 0)
fruit_holdings$ptocc1 <-
  ifelse(fruit_holdings[occupier1_pt_gthalf] > 0, 1, 0)
fruit_holdings$lptocc1 <-
  ifelse(fruit_holdings[occupier1_pt_lthalf] > 0, 1, 0)
fruit_holdings$nwrkocc1 <-
  ifelse(fruit_holdings[occupier1_no_work] > 0, 1, 0)
fruit_holdings$ftocc2 <-
  ifelse(fruit_holdings[occupier2_ft] > 0, 1, 0)
fruit_holdings$ptocc2 <-
  ifelse(fruit_holdings[occupier2_pt_gthalf] > 0, 1, 0)
fruit_holdings$lptocc2 <-
  ifelse(fruit_holdings[occupier2_pt_lthalf] > 0, 1, 0)
fruit_holdings$nwrkocc2 <-
  ifelse(fruit_holdings[occupier2_no_work] > 0, 1, 0)

full <-
  c(labour_ft_m_bp,
    labour_ft_m_hired,
    labour_ft_f_bp,
    labour_ft_f_hired)
fruit_holdings$full <- rowSums(fruit_holdings[full], na.rm = TRUE)
fruit_holdings$full <-
  fruit_holdings$ftocc1 + fruit_holdings$ftocc2 + fruit_holdings$full

part <-
  c(labour_pt_m_bp,
    labour_pt_m_hired,
    labour_pt_f_bp,
    labour_pt_f_hired)
fruit_holdings$part <- rowSums(fruit_holdings[part], na.rm = TRUE)
fruit_holdings$part <-
  fruit_holdings$ptocc1 + fruit_holdings$ptocc2 + fruit_holdings$part


casual <- c(labour_cas_m, labour_cas_f)
fruit_holdings$casual <-
  rowSums(fruit_holdings[casual], na.rm = TRUE)
fruit_holdings$casual <-
  fruit_holdings$lptocc1 + fruit_holdings$lptocc2 + fruit_holdings$casual

#nonreg <- c(labour_non_fam_non_reg, labour_agri_contract, labour_non_EU_migrant)
fruit_holdings$nonreg <-
  fruit_holdings[labour_non_fam_non_reg] + fruit_holdings[labour_agri_contract] + fruit_holdings[labour_non_EU_migrant] +
  fruit_holdings[labour_EU_migrant]

fruit_holdings$full_lab <- fruit_holdings$full
fruit_holdings$part_lab <- fruit_holdings$part * 0.5
fruit_holdings$casual_lab <- fruit_holdings$casual * 0.25
fruit_holdings$nonreg_lab <- fruit_holdings$nonreg * 8 / 1900
fruit_holdings$lab_returned <-
  (
    fruit_holdings$full_lab + fruit_holdings$part_lab + fruit_holdings$casual_lab + fruit_holdings$nonreg_lab
  )
fruit_holdings$lab_returned[is.na(fruit_holdings$lab_returned)] <- 0

#Sum labour by brn
brn_fruit <-
  fruit_holdings %>% ungroup() %>% select(brn, sumfruit, slr_fruit, lab_returned, all_of(c(labour_cas_f, labour_cas_m))) %>% group_by(brn) %>% filter(brn >
                                                                                                                                                        0) %>% arrange(brn)
brn_fruit <-
  brn_fruit %>% summarise(
    freq = n(),
    sumfruit_sum = sum(sumfruit),
    slr_fruit_sum = sum(slr_fruit),
    lab_returned_sum = sum(lab_returned)
  )

#holdings with more than 2ha of fruit
large_fruit <-
  fruit_holdings %>% filter(sumfruit > 2 &
                              survtype != "SAF_only") %>% arrange(brn, by_group = FALSE)

check_labour <- left_join(large_fruit, brn_fruit, by = "brn")

check_labour <-
  check_labour %>% mutate(diff_lab = slr_fruit_sum - lab_returned_sum,
                          diff_lab_nobrn = slr_fruit - lab_returned) %>% arrange(desc(diff_lab))

check_labour <-
  check_labour %>%  select(
    parish,
    holding,
    survtype,
    submisType,
    brn,
    land_data,
    saf_data,
    sumfruit,
    sumfruit_sum,
    slr_fruit,
    slr_fruit_sum,
    lab_returned,
    lab_returned_sum,
    diff_lab,
    diff_lab_nobrn,
    labour_cas_f,
    labour_cas_m
  )

#Migrant check errors
check_labour$err39 <-
  ifelse(
    ifelse(
      check_labour$brn >= 0,
      abs(check_labour$diff_lab) > 30 &
        check_labour$lab_returned_sum < 0.3 * check_labour$slr_fruit_sum,
      0
    ),
    1,
    0
  )
check_labour$err40 <-
  ifelse(
    ifelse(
      check_labour$brn >= 0,
      abs(check_labour$diff_lab) > 30 &
        check_labour$lab_returned_sum > 3 * check_labour$slr_fruit_sum,
      0
    ),
    1,
    0
  )
check_labour$err41 <-
  ifelse(
    ifelse(
      check_labour$brn < 0 |
        is.na(check_labour$brn),
      abs(check_labour$diff_lab_nobrn) > 30 &
        check_labour$lab_returned < 0.3 * check_labour$slr_fruit,
      0
    ),
    1,
    0
  )
check_labour$err42 <-
  ifelse(
    ifelse(
      check_labour$brn < 0 |
        is.na(check_labour$brn),
      abs(check_labour$diff_lab_nobrn) > 30 &
        check_labour$lab_returned > 3 * check_labour$slr_fruit,
      0
    ),
    1,
    0
  )

#Validation Outputs---------------------------------------------------------------------------------------

new_validations <-
  c(
    "err37",
    "err38",
    "err43",
    "err44",
    "err45",
    "err46",
    "err47",
    "err48",
    "err49",
    "err50",
    "err55",
    "err56",
    "err57",
    "err58",
    "err59",
    "err60",
    "err61"
  )
migrant_labour_validations <- c("err39", "err40", "err41", "err42")
all_validations <-
  c(
    "err1",
    "err2",
    "err3",
    "err4",
    "err5",
    "err7",
    "err9",
    "err10",
    "err11",
    "err12",
    "err13",
    "err14",
    "err15",
    "err16",
    "err17",
    "err18",
    "err19",
    "err20",
    "err21",
    "err22",
    "err23",
    "err24",
    "err25",
    "err26",
    "err30",
    "err31",
    "err32",
    "err33",
    "err36",
    new_validations,
    migrant_labour_validations
  )

#dataframe of observations (holdings) that have passed validations
clean_JAC <-
  all_JAC_form %>% filter(if_all(starts_with("err"), ~ . == 0)) %>% select(-matches("err|total"))





#Validations summary--------------------------------------------------------------------------------------------------------------------

#dataframe of observations (holdings) that have failed the new validations -
JAC_validation_errors <-
  all_JAC_form %>% select(parish, holding, submisType, any_of(all_validations)) %>%  filter(if_any(starts_with("err"), ~ . !=
                                                                                                     0))
JAC_validation_error_summary <-
  JAC_validation_errors %>% ungroup() %>% group_by(submisType) %>%   select(starts_with("err")) %>% summarize(across(everything(), sum, na.rm = TRUE))
JAC_validation_error_summary <-
  cbind(
    JAC_validation_error_summary,
    total_cases_with_errors = rowSums(JAC_validation_error_summary[names(JAC_validation_error_summary) %in% all_validations], na.rm =
                                        TRUE)
  )
JAC_validation_error_summary <-
  as.data.frame(t(JAC_validation_error_summary)) %>%
  janitor::row_to_names(1)
colnames(JAC_validation_error_summary) <-
  paste0("SubmisType", colnames(JAC_validation_error_summary))

#soft fruit migrant labour check
check_labour_error_summary <-
  check_labour %>% ungroup() %>%  select(starts_with("err")) %>% summarize(across(everything(), sum, na.rm = TRUE)) %>% pivot_longer(cols = everything(),
                                                                                                                                     names_to = "error",
                                                                                                                                     values_to = "count")
# R Markdown Tables -------------------------------------------------------

##----err 1----------------------------------------------------------------------------------------------
##@knitr err1




err1 <-
  all_JAC_form %>% select(
    parish,
    holding,
    survtype,
    submisType,
    land_data,
    saf_data,
    all_of(c(area_own, area_rent, total_area)),
    err1,
    err1_diff
  )
err1 <-
  err1 %>%  mutate(error = ifelse(err1 == 1 |
                                    is.na(err1), "Error", "No Error")) %>% filter(error == "Error")

##----err 37----------------------------------------------------------------------------------------------
##@knitr err37
err37 <-
  all_JAC_form %>% select(parish,
                          holding,
                          survtype,
                          submisType,
                          land_data,
                          saf_data,
                          all_of(c(area_own, area_rent, total_area)),
                          err37)
err37 <-
  err37 %>%  mutate(error = ifelse(err37 == 1 |
                                     is.na(err37), "Error", "No Error")) %>% filter(error == "Error")


##----err 38----------------------------------------------------------------------------------------------
##@knitr err38

err38 <-
  all_JAC_form %>% select(parish,
                          holding,
                          survtype,
                          submisType,
                          land_data,
                          saf_data,
                          all_of(c(area_own, area_rent, total_area)),
                          err38)
err38 <-
  err38 %>%  mutate(error = ifelse(err38 == 1 |
                                     is.na(err38), "Error", "No Error")) %>% filter(error == "Error")

##----err 56----------------------------------------------------------------------------------------------
##@knitr err56

#err56<- all_JAC_form %>% select(parish, holding, survtype, submisType, land_data, saf_data, all_of(c(rps_rent, area_rent)), err56)
#err56 <-err56 %>%  mutate(error=ifelse(err56== 1 | is.na(err56), "Error", "No Error")) %>% filter(error == "Error")

##----err 57----------------------------------------------------------------------------------------------
##@knitr err57

err57 <-
  all_JAC_form %>% select(
    parish,
    holding,
    survtype,
    submisType,
    land_data,
    saf_data,
    all_of(c(rps_rent, area_rent)),
    err57,
    err57_diff
  )
err57 <-
  err57 %>%  mutate(error = ifelse(err57 == 1 |
                                     is.na(err57), "Error", "No Error")) %>% filter(error == "Error")

##----err 58----------------------------------------------------------------------------------------------
##@knitr err58

#err58<- all_JAC_form %>% select(parish, holding, survtype, submisType, land_data, saf_data, all_of(c(rps_own, area_own)), err58)
#err58 <-err58 %>%  mutate(error=ifelse(err58== 1 | is.na(err58), "Error", "No Error")) %>% filter(error == "Error")

##----err 59----------------------------------------------------------------------------------------------
##@knitr err59

err59 <-
  all_JAC_form %>% select(
    parish,
    holding,
    survtype,
    submisType,
    land_data,
    saf_data,
    all_of(c(rps_own, area_own)),
    err59,
    err59_diff
  )
err59 <-
  err59 %>%  mutate(error = ifelse(err59 == 1 |
                                     is.na(err59), "Error", "No Error")) %>% filter(error == "Error")

##----err 61----------------------------------------------------------------------------------------------
##@knitr err61

err61 <-
  all_JAC_form %>% select(
    parish,
    holding,
    survtype,
    submisType,
    land_data,
    saf_data,
    total14,
    total14rps,
    all_of(c(area_own, area_rent, rps_own, rps_rent)),
    err61,
    err61_diff
  )
err61 <-
  err61 %>%  mutate(error = ifelse(err61 == 1 |
                                     is.na(err61), "Error", "No Error")) %>% filter(error == "Error")
##----err 2----------------------------------------------------------------------------------------------
##@knitr err2
err2 <-
  all_JAC_form %>% select(parish,
                          holding,
                          survtype,
                          submisType,
                          land_data,
                          saf_data,
                          all_of(c(seas_total_area_in, area_rent)),
                          err2)
err2 <-
  err2 %>%  mutate(error = ifelse(err2 == 1 |
                                    is.na(err2), "Error", "No Error")) %>% filter(error == "Error")

##----err 3----------------------------------------------------------------------------------------------
##@knitr err3

err3 <-
  all_JAC_form %>% select(
    parish,
    holding,
    survtype,
    submisType,
    land_data,
    saf_data,
    all_of(c(
      seas_total_area_in, seas_rent_diff_in, seas_rent_same_in
    )),
    err3,
    err3_diff
  )
err3 <-
  err3 %>%  mutate(error = ifelse(err3 == 1 |
                                    is.na(err3), "Error", "No Error")) %>% filter(error == "Error")

##----err 43----------------------------------------------------------------------------------------------
##@knitr err43

err43 <-
  all_JAC_form %>% select(
    parish,
    holding,
    survtype,
    submisType,
    land_data,
    saf_data,
    all_of(c(seas_area_rent_out, total_area)),
    err43,
    err43_diff
  )
err43 <-
  err43 %>%  mutate(error = ifelse(err43 == 1 |
                                     is.na(err43), "Error", "No Error")) %>% filter(error == "Error")



##----err 4----------------------------------------------------------------------------------------------
##@knitr err4

err4 <-
  all_JAC_form %>% select(parish,
                          holding,
                          survtype,
                          submisType,
                          land_data,
                          saf_data,
                          all_of(c(other_crops, other_crops_text)),
                          err4)
err4 <-
  err4 %>%  mutate(error = ifelse(err4 == 1 |
                                    is.na(err4), "Error", "No Error")) %>% filter(error == "Error")

##----err 44----------------------------------------------------------------------------------------------
##@knitr err44

err44 <-
  all_JAC_form %>% select(parish,
                          holding,
                          survtype,
                          submisType,
                          land_data,
                          saf_data,
                          all_of(c(other_crops, other_crops_text)),
                          err44)
err44 <-
  err44 %>%  mutate(error = ifelse(err44 == 1 |
                                     is.na(err44), "Error", "No Error")) %>% filter(error == "Error")

##----err 5----------------------------------------------------------------------------------------------
##@knitr err5

err5 <-
  all_JAC_form %>% select(
    parish,
    holding,
    survtype,
    submisType,
    land_data,
    saf_data,
    all_of(
      c(
        total_crops_grass,
        rough_graze,
        woodland,
        other_land,
        total_land
      )
    ),
    total7,
    err5,
    err5_diff
  )
err5 <-
  err5 %>%  mutate(error = ifelse(err5 == 1 |
                                    is.na(err5), "Error", "No Error")) %>% filter(error == "Error")

##----err 6----------------------------------------------------------------------------------------------
##@knitr err6
#err6 <- all_JAC_form %>% select(parish, holding, survtype, submisType, land_data, saf_data, all_of(c(total2)), total2, err6)
#err6 <-err6 %>%  mutate(error=ifelse(err6== 1 | is.na(err6), "Error", "No Error"))%>% filter(error == "Error")

##----err 7----------------------------------------------------------------------------------------------
##@knitr err7

err7 <-
  all_JAC_form %>% select(
    parish,
    holding,
    survtype,
    submisType,
    land_data,
    saf_data,
    all_of(c(all_veg_saf, all_veg_open)),
    err7,
    err7_diff
  )
err7 <-
  err7 %>%  mutate(error = ifelse(err7 == 1 |
                                    is.na(err7), "Error", "No Error")) %>% filter(error == "Error")

##----err 8----------------------------------------------------------------------------------------------
##@knitr err8

#err8 <- all_JAC_form %>% select(parish, holding, survtype, submisType, land_data, saf_data, all_of(c(all_fruit_saf, total3)), total3, err8)
#err8 <-err8 %>%  mutate(error=ifelse(err8== 1 | is.na(err8), "Error", "No Error"))%>% filter(error == "Error")

##----err 9----------------------------------------------------------------------------------------------
##@knitr err9
err9 <-
  all_JAC_form %>% select(
    parish,
    holding,
    survtype,
    submisType,
    land_data,
    saf_data,
    all_of(c(soft_fruits, all_fruit_saf)),
    err9,
    err9_diff
  )
err9 <-
  err9 %>%  mutate(error = ifelse(err9 == 1 |
                                    is.na(err9), "Error", "No Error")) %>% filter(error == "Error")

##----err 10----------------------------------------------------------------------------------------------
##@knitr err10
#change all_flow_bulb_21 to all_flow_bulb for 2023
err10 <-
  all_JAC_form %>% select(
    parish,
    holding,
    survtype,
    submisType,
    land_data,
    saf_data,
    all_of(c(all_flow_bulb, total4)),
    err10,
    err10_diff
  )
err10 <-
  err10 %>%  mutate(error = ifelse(err10 == 1 |
                                     is.na(err10), "Error", "No Error")) %>% filter(error == "Error")

##----err 11----------------------------------------------------------------------------------------------
##@knitr err11
err11 <-
  all_JAC_form %>% select(
    parish,
    holding,
    survtype,
    submisType,
    land_data,
    saf_data,
    all_of(c(
      total5b, tot_open_plastic, total6b, tot_solid_glass
    )),
    err11,
    err11open_diff,
    err11solid_diff
  )
err11 <-
  err11 %>%  mutate(error = ifelse(err11 == 1 |
                                     is.na(err11), "Error", "No Error")) %>% filter(error == "Error")

##----err 12----------------------------------------------------------------------------------------------
##@knitr err12
err12 <-
  all_JAC_form %>% select(
    parish,
    holding,
    survtype,
    submisType,
    land_data,
    saf_data,
    all_of(c(
      total5b, tot_open_plastic, total6b, tot_solid_glass
    )),
    err12,
    err12_diff
  )
err12 <-
  err12 %>%  mutate(error = ifelse(err12 == 1 |
                                     is.na(err12), "Error", "No Error")) %>% filter(error == "Error")

##----err 45----------------------------------------------------------------------------------------------
##@knitr err45
err45 <-
  all_JAC_form %>% select(
    parish,
    holding,
    survtype,
    submisType,
    land_data,
    saf_data,
    all_of(c(total_land, total_area, rps_total_area)),
    total13,
    err45,
    err45_diff
  )
err45 <-
  err45 %>%  mutate(error = ifelse(err45 == 1 |
                                     is.na(err45), "Error", "No Error")) %>% filter(error == "Error")

##----err 46----------------------------------------------------------------------------------------------
##@knitr err46
err46 <-
  all_JAC_form %>% select(
    parish,
    holding,
    survtype,
    submisType,
    land_data,
    saf_data,
    all_of(c(total_land, total_area, rps_total_area)),
    err46,
    err46_diff
  )
err46 <-
  err46 %>%  mutate(error = ifelse(err46 == 1 |
                                     is.na(err46), "Error", "No Error")) %>% filter(error == "Error")

##----err 13----------------------------------------------------------------------------------------------
##@knitr err13
#change total10_21 to total10 for 2023
err13 <-
  all_JAC_form %>% select(
    parish,
    holding,
    survtype,
    submisType,
    land_data,
    saf_data,
    all_of(c(total10, total_pig)),
    err13,
    err13_diff
  )
err13 <-
  err13 %>%  mutate(error = ifelse(err13 == 1|
                                     is.na(err13) ,"Error", "No Error")) %>% filter(error == "Error")

##----err 14----------------------------------------------------------------------------------------------
##@knitr err14
#change total10_21 to total10 for 2023
err14 <-
  all_JAC_form %>% select(parish,
                          holding,
                          survtype,
                          submisType,
                          land_data,
                          saf_data,
                          all_of(c(total10)),
                          err14)
err14 <-
  err14 %>%  mutate(error = ifelse(err14 == 1 |
                                     is.na(err14), "Error", "No Error")) %>% filter(error == "Error")

##----err 15----------------------------------------------------------------------------------------------
##@knitr err15
err15 <-
  all_JAC_form %>% select(
    parish,
    holding,
    survtype,
    submisType,
    land_data,
    saf_data,
    all_of(c(total9, total_sheep)),
    total9,
    err15,
    err15_diff
  )
err15 <-
  err15 %>%  mutate(error = ifelse(err15 == 1 |
                                     is.na(err15), "Error", "No Error")) %>% filter(error == "Error")

##----err 16----------------------------------------------------------------------------------------------
##@knitr err16
err16 <-
  all_JAC_form %>% select(parish,
                          holding,
                          survtype,
                          submisType,
                          land_data,
                          saf_data,
                          all_of(c(total9, total_sheep)),
                          total9,
                          err16)
err16 <-
  err16 %>%  mutate(error = ifelse(err16 == 1 |
                                     is.na(err16), "Error", "No Error")) %>% filter(error == "Error")

##----err 17----------------------------------------------------------------------------------------------
##@knitr err17
err17 <-
  all_JAC_form %>% select(
    parish,
    holding,
    survtype,
    submisType,
    land_data,
    saf_data,
    all_of(c(total11, total_poultry)),
    total11,
    err17,
    err17_diff
  )
err17 <-
  err17 %>%  mutate(error = ifelse(err17 == 1 |
                                     is.na(err17), "Error", "No Error")) %>% filter(error == "Error")

##----err 18----------------------------------------------------------------------------------------------
##@knitr err18
err18 <-
  all_JAC_form %>% select(parish,
                          holding,
                          survtype,
                          submisType,
                          land_data,
                          saf_data,
                          all_of(c(total11, total_poultry)),
                          err18)
err18 <-
  err18 %>%  mutate(error = ifelse(err18 == 1 |
                                     is.na(err18), "Error", "No Error")) %>% filter(error == "Error")

##----err 19----------------------------------------------------------------------------------------------
##@knitr err19
err19 <-
  all_JAC_form %>% select(parish,
                          holding,
                          survtype,
                          submisType,
                          land_data,
                          saf_data,
                          all_of(
                            c(
                              first_hens,
                              moulted_hens,
                              pullets,
                              layer_chicks,
                              table_chicks,
                              cocks,
                              broilers
                            )
                          ),
                          err19)
err19 <-
  err19 %>%  mutate(error = ifelse(err19 == 1 |
                                     is.na(err19), "Error", "No Error")) %>% filter(error == "Error")

##----err 20----------------------------------------------------------------------------------------------
##@knitr err20
err20 <-
  all_JAC_form %>% select(parish,
                          holding,
                          survtype,
                          submisType,
                          land_data,
                          saf_data,
                          all_of(deer),
                          err20)
err20 <-
  err20 %>%  mutate(error = ifelse(err20 == 1 |
                                     is.na(err20), "Error", "No Error")) %>% filter(error == "Error")

##----err 21----------------------------------------------------------------------------------------------
##@knitr err21
err21 <-
  all_JAC_form %>% select(parish,
                          holding,
                          survtype,
                          submisType,
                          land_data,
                          saf_data,
                          all_of(c(alpacas, llamas, other_camelids)),
                          err21)
err21 <-
  err21 %>%  mutate(error = ifelse(err21 == 1 |
                                     is.na(err21), "Error", "No Error")) %>% filter(error == "Error")

##----err 22----------------------------------------------------------------------------------------------
##@knitr err22
err22 <-
  all_JAC_form %>% select(parish,
                          holding,
                          survtype,
                          submisType,
                          land_data,
                          saf_data,
                          all_of(c(hives)),
                          err22)
err22 <-
  err22 %>%  mutate(error = ifelse(err22 == 1 |
                                     is.na(err22), "Error", "No Error")) %>% filter(error == "Error")

##----err 23----------------------------------------------------------------------------------------------
##@knitr err23
err23 <-
  all_JAC_form %>% select(parish,
                          holding,
                          survtype,
                          submisType,
                          land_data,
                          saf_data,
                          all_of(c(other_livestock)),
                          err23)
err23 <-
  err23 %>%  mutate(error = ifelse(err23 == 1 |
                                     is.na(err23), "Error", "No Error")) %>% filter(error == "Error")

##----err 24----------------------------------------------------------------------------------------------
##@knitr err24
err24 <-
  all_JAC_form %>% select(parish,
                          holding,
                          survtype,
                          submisType,
                          land_data,
                          saf_data,
                          all_of(c(other_livestock, other_livestock_txt)),
                          err24)
err24 <-
  err24 %>%  mutate(error = ifelse(err24 == 1 |
                                     is.na(err24), "Error", "No Error")) %>% filter(error == "Error")

##----err 47----------------------------------------------------------------------------------------------
##@knitr err47
err47 <-
  all_JAC_form %>% select(parish,
                          holding,
                          survtype,
                          submisType,
                          land_data,
                          saf_data,
                          all_of(c(other_livestock, other_livestock_txt)),
                          err47)
err47 <-
  err47 %>%  mutate(error = ifelse(err47 == 1 |
                                     is.na(err47), "Error", "No Error")) %>% filter(error == "Error")

##----err 26----------------------------------------------------------------------------------------------
##@knitr err26
# err26 <- all_JAC_form %>% select(parish, holding, survtype, submisType, land_data, saf_data, all_of(c(female_goat_kids, other_female_goat, other_goats_kids)), err26)
# err26 <-err26 %>%  mutate(error=ifelse(err26== 1 | is.na(err26), "Error", "No Error"))%>% filter(error == "Error")

##----err 27----------------------------------------------------------------------------------------------
##@knitr err27
err27 <-
  all_JAC_form %>% select(parish,
                          holding,
                          survtype,
                          submisType,
                          land_data,
                          saf_data,
                          all_of(c(horse_pony, donkeys)),
                          err27)
err27 <-
  err27 %>%  mutate(error = ifelse(err27 == 1 |
                                     is.na(err27), "Error", "No Error")) %>% filter(error == "Error")

##----err 28----------------------------------------------------------------------------------------------
# ##@knitr err28
err28 <-
  all_JAC_form %>% select(parish,
                          holding,
                          survtype,
                          submisType,
                          land_data,
                          saf_data,
                          all_of(c(goats_kids)),
                          err28)
err28 <-
  err28 %>%  mutate(error = ifelse(err28 == 1 |
                                     is.na(err28), "Error", "No Error")) %>% filter(error == "Error")

##----err 60----------------------------------------------------------------------------------------------
# ##@knitr err60
err60 <-
  all_JAC_form %>% select(parish,
                          holding,
                          survtype,
                          submisType,
                          land_data,
                          saf_data,
                          all_of(c(legal_fin_resp, other_legal_return)),
                          err60)
err60 <-
  err60 %>%  mutate(error = ifelse(err60 == 1 |
                                     is.na(err60), "Error", "No Error")) %>% filter(error == "Error")


##----err 29----------------------------------------------------------------------------------------------
##@knitr err29
err29 <-
  all_JAC_form %>% select(parish,
                          holding,
                          survtype,
                          submisType,
                          land_data,
                          saf_data,
                          all_of(
                            c(
                              other_legal_return,
                              occupier1_ft,
                              occupier1_pt_gthalf,
                              occupier1_pt_lthalf,
                              occupier1_no_work,
                              occupier2_ft,
                              occupier2_pt_gthalf,
                              occupier2_pt_lthalf,
                              occupier2_no_work
                            )
                          ),
                          err29)
err29 <-
  err29 %>%  mutate(error = ifelse(err29 == 1 |
                                     is.na(err29), "Error", "No Error")) %>% filter(error == "Error")

##----err 52----------------------------------------------------------------------------------------------
##@knitr err52
err52 <-
  all_JAC_form %>% select(parish,
                          holding,
                          survtype,
                          submisType,
                          land_data,
                          saf_data,
                          all_of(
                            c(
                              other_legal_return,
                              occupier1_ft,
                              occupier1_pt_gthalf,
                              occupier1_pt_lthalf,
                              occupier1_no_work,
                              occupier2_ft,
                              occupier2_pt_gthalf,
                              occupier2_pt_lthalf,
                              occupier2_no_work
                            )
                          ),
                          err52)
err52 <-
  err52 %>%  mutate(error = ifelse(err52 == 1 |
                                     is.na(err52), "Error", "No Error")) %>% filter(error == "Error")

##----err 53----------------------------------------------------------------------------------------------
##@knitr err53
err53 <-
  all_JAC_form %>% select(parish,
                          holding,
                          survtype,
                          submisType,
                          land_data,
                          saf_data,
                          all_of(
                            c(
                              occupier2_legal_fin,
                              occupier2_ft,
                              occupier2_pt_gthalf,
                              occupier2_pt_lthalf,
                              occupier2_no_work
                            )
                          ),
                          err53)
err53 <-
  err53 %>%  mutate(error = ifelse(err53 == 1 |
                                     is.na(err53), "Error", "No Error")) %>% filter(error == "Error")


##----err 54----------------------------------------------------------------------------------------------
##@knitr err54
err54 <-
  all_JAC_form %>% select(parish,
                          holding,
                          survtype,
                          submisType,
                          land_data,
                          saf_data,
                          all_of(
                            c(
                              occupier2_legal_fin,
                              occupier2_ft,
                              occupier2_pt_gthalf,
                              occupier2_pt_lthalf,
                              occupier2_no_work
                            )
                          ),
                          err54)
err54 <-
  err54 %>%  mutate(error = ifelse(err54 == 1 |
                                     is.na(err54), "Error", "No Error")) %>% filter(error == "Error")


##----err 30----------------------------------------------------------------------------------------------
##@knitr err30
err30 <-
  all_JAC_form %>% select(parish,
                          holding,
                          survtype,
                          submisType,
                          land_data,
                          saf_data,
                          all_of(
                            c(
                              occupier1_ft,
                              occupier1_pt_gthalf,
                              occupier1_pt_lthalf,
                              occupier1_no_work
                            )
                          ),
                          err30)
err30 <-
  err30 %>%  mutate(error = ifelse(err30 == 1 |
                                     is.na(err30), "Error", "No Error")) %>% filter(error == "Error")

##----err 31----------------------------------------------------------------------------------------------
##@knitr err31
err31 <-
  all_JAC_form %>% select(parish,
                          holding,
                          survtype,
                          submisType,
                          land_data,
                          saf_data,
                          all_of(
                            c(
                              occupier2_ft,
                              occupier2_pt_gthalf,
                              occupier2_pt_lthalf,
                              occupier2_no_work
                            )
                          ),
                          err31)
err31 <-
  err31 %>%  mutate(error = ifelse(err31 == 1 |
                                     is.na(err31), "Error", "No Error")) %>% filter(error == "Error")

##----err 48----------------------------------------------------------------------------------------------
##@knitr err48
err48 <-
  all_JAC_form %>% select(parish,
                          holding,
                          survtype,
                          submisType,
                          land_data,
                          saf_data,
                          all_of(
                            c(
                              occupier1_female,
                              occupier1_male,
                              occupier2_female,
                              occupier2_male
                            )
                          ),
                          err48)
err48 <-
  err48 %>%  mutate(error = ifelse(err48 == 1 |
                                     is.na(err48), "Error", "No Error")) %>% filter(error == "Error")

##----err 49----------------------------------------------------------------------------------------------
##@knitr err49
err49 <-
  all_JAC_form %>% select(parish,
                          holding,
                          survtype,
                          submisType,
                          land_data,
                          saf_data,
                          all_of(
                            c(
                              occupier1_female,
                              occupier1_male,
                              occupier1_ft,
                              occupier1_pt_gthalf,
                              occupier1_pt_lthalf,
                              occupier1_no_work
                            )
                          ),
                          err49)
err49 <-
  err49 %>%  mutate(error = ifelse(err49 == 1 |
                                     is.na(err49), "Error", "No Error")) %>% filter(error == "Error")

##----err 50----------------------------------------------------------------------------------------------
##@knitr err50
err50 <-
  all_JAC_form %>% select(parish,
                          holding,
                          survtype,
                          submisType,
                          land_data,
                          saf_data,
                          all_of(
                            c(
                              occupier1_female,
                              occupier1_male,
                              occupier1_ft,
                              occupier1_pt_gthalf,
                              occupier1_pt_lthalf,
                              occupier1_no_work
                            )
                          ),
                          err50)
err50 <-
  err50 %>%  mutate(error = ifelse(err50 == 1 |
                                     is.na(err50), "Error", "No Error")) %>% filter(error == "Error")


##----err 51----------------------------------------------------------------------------------------------
##@knitr err51
err51 <-
  all_JAC_form %>% select(parish,
                          holding,
                          survtype,
                          submisType,
                          land_data,
                          saf_data,
                          all_of(c(occupier1_year, occupier2_year)),
                          err51)
err51 <-
  err51 %>%  mutate(error = ifelse(err51 == 1 |
                                     is.na(err51), "Error", "No Error")) %>% filter(error == "Error")

##----err 55----------------------------------------------------------------------------------------------
##@knitr err55
err55 <-
  all_JAC_form %>% select(parish,
                          holding,
                          survtype,
                          submisType,
                          land_data,
                          saf_data,
                          all_of(
                            c(
                              occupier1_ft,
                              occupier1_pt_gthalf,
                              occupier1_pt_lthalf,
                              occupier1_no_work,
                              occupier1_male,
                              occupier1_female,
                              occupier2_ft,
                              occupier2_pt_gthalf,
                              occupier2_pt_lthalf,
                              occupier2_no_work,
                              occupier2_female,
                              occupier2_male
                            )
                          ),
                          err55)
err55 <-
  err55 %>%  mutate(error = ifelse(err55 == 1 |
                                     is.na(err55), "Error", "No Error")) %>% filter(error == "Error")

##----err 32----------------------------------------------------------------------------------------------
##@knitr err32
err32 <-
  all_JAC_form %>% select(parish,
                          holding,
                          survtype,
                          submisType,
                          land_data,
                          saf_data,
                          all_of(c(total12, total_labour)),
                          total12,
                          err32)
err32 <-
  err32 %>%  mutate(error = ifelse(err32 == 1 |
                                     is.na(err32), "Error", "No Error")) %>% filter(error == "Error")


##----err 33----------------------------------------------------------------------------------------------
##@knitr err33
err33 <-
  all_JAC_form %>% select(parish,
                          holding,
                          survtype,
                          submisType,
                          land_data,
                          saf_data,
                          all_of(c(total12, total_labour)),
                          err33)
err33 <-
  err33 %>%  mutate(error = ifelse(err33 == 1 |
                                     is.na(err33), "Error", "No Error")) %>% filter(error == "Error")


# ##----err 34----------------------------------------------------------------------------------------------
# ##@knitr err34
# err34 <- all_JAC_form %>% select(parish, holding, survtype, submisType, land_data, saf_data, all_of(c(owned_croft, area_own)), err34)
# err34 <-err34 %>%  mutate(error=ifelse(err34== 1 | is.na(err34), "Error", "No Error"))%>% filter(error == "Error")
#
# ##----err 35----------------------------------------------------------------------------------------------
# ##@knitr err35
# err35 <- all_JAC_form %>% select(parish, holding, survtype, submisType, land_data, saf_data, all_of((owned_croft)), err35)
# err35 <-err35 %>%  mutate(error=ifelse(err35== 1 | is.na(err35), "Error", "No Error"))%>% filter(error == "Error")

##----err 36----------------------------------------------------------------------------------------------
##@knitr err36
#err36 <- all_JAC_form %>% select(parish, holding, survtype, submisType, land_data, saf_data, all_of((rented_croft)), err36)
#err36 <-err36 %>%  mutate(error=ifelse(err36== 1 | is.na(err36), "Error", "No Error"))%>% filter(error == "Error")

##----err 39----------------------------------------------------------------------------------------------
##@knitr err39
err39 <-
  check_labour %>% select(
    parish,
    holding,
    survtype,
    submisType,
    land_data,
    saf_data,
    brn,
    land_data,
    saf_data,
    sumfruit,
    sumfruit_sum,
    slr_fruit,
    slr_fruit_sum,
    lab_returned,
    lab_returned_sum,
    all_of(c(labour_cas_f, labour_cas_m)),
    err39
  )
err39 <-
  err39 %>%  mutate(error = ifelse(err39 == 1 |
                                     is.na(err39), "Error", "No Error")) %>% filter(error == "Error")


##----err 40----------------------------------------------------------------------------------------------
##@knitr err40
err40 <-
  check_labour %>% select(
    parish,
    holding,
    survtype,
    submisType,
    land_data,
    saf_data,
    brn,
    land_data,
    saf_data,
    sumfruit,
    sumfruit_sum,
    slr_fruit,
    slr_fruit_sum,
    lab_returned,
    lab_returned_sum,
    all_of(c(labour_cas_f, labour_cas_m)),
    err40
  )
err40 <-
  err40 %>%  mutate(error = ifelse(err40 == 1 |
                                     is.na(err40), "Error", "No Error")) %>% filter(error == "Error")

##----err 41----------------------------------------------------------------------------------------------
##@knitr err41
err41 <-
  check_labour %>% select(
    parish,
    holding,
    survtype,
    submisType,
    land_data,
    saf_data,
    brn,
    land_data,
    saf_data,
    sumfruit,
    sumfruit_sum,
    slr_fruit,
    slr_fruit_sum,
    lab_returned,
    lab_returned_sum,
    all_of(c(labour_cas_f, labour_cas_m)),
    err41
  )
err41 <-
  err41 %>%  mutate(error = ifelse(err41 == 1 |
                                     is.na(err41), "Error", "No Error")) %>% filter(error == "Error")

##----err 42----------------------------------------------------------------------------------------------
##@knitr err42
err42 <-
  check_labour %>% select(
    parish,
    holding,
    survtype,
    submisType,
    land_data,
    saf_data,
    brn,
    land_data,
    saf_data,
    sumfruit,
    sumfruit_sum,
    slr_fruit,
    slr_fruit_sum,
    lab_returned,
    lab_returned_sum,
    all_of(c(labour_cas_f, labour_cas_m)),
    err42
  )
err42 <-
  err42 %>%  mutate(error = ifelse(err42 == 1 |
                                     is.na(err42), "Error", "No Error")) %>% filter(error == "Error")



##----err-summary----------------------------------------------------------------------------------------------
##@knitr err-summary

JAC_validation_error_summary
#JAC_validation_error_summary_NA
check_labour_error_summary
#JAC_new_validation_error_summary




#Ops outputs------------------------------------------------------------------------

#select error columns from dataset and filter for cases with errors
main_validations <-
  all_JAC_form %>% select(
    parish,
    holding,
    any_of(all_validations),
    err46_diff,
    err45_diff,
    err59_diff,
    err57_diff,
    err61_diff
  )
#list of relevant errors to filter
relevant_errors <-
  names(main_validations[names(main_validations) %in% all_validations])
main_validations <-
  main_validations %>% filter(if_any(relevant_errors) != 0)

#samee for migrant labour checks
migrant_errors <-
  check_labour %>%  select(parish, holding, any_of(all_validations))
relevant_merrors <-
  names(migrant_errors[names(migrant_errors) %in% all_validations])
migrant_errors <-
  migrant_errors %>% filter(if_any(relevant_merrors) != 0)



#Work item hierarchy------------------------------------------------------

#include err46_diff, err_45_diff and err61_diff to arrange by largest discrepancies
#trimmed off err13, err15, err17, err32
priority <-
  c(
    "err46_diff",
    "err46",
    "err45",
    "err45_diff",
    "err4",
    "err44",
    "err24",
    "err47",
    "err49",
    "err50",
    "err30",
    "err31",
    "err60",
    "err54",
    "err53",
    "err48",
    "err51",
    "err55",
    "err11",
    "err1",
    "err37",
    "err38",
    "err61",
    "err61_diff",
    "err52"
  )

#arrange columns according to work item priority order
main_validations <-
  main_validations %>% select(parish, holding, any_of(priority))




#manual sorting rows by order of priority - todo: loop through priority list
#comment out errors that are zero - won't work otherwise.

main_validations <-
  main_validations %>% arrange(
    desc(err46),
    desc(err46_diff),
    desc(err45),
    desc(err45_diff),
    desc(err4),
    desc(err44),
    desc(err24),
    desc(err47),
    desc(err49),
    desc(err50),
    desc(err30),
    desc(err31),
    desc(err60),
    desc(err48),
    #desc(err51),
    desc(err55),
    desc(err1),
    desc(err37),
    desc(err38),
    desc(err61),
    desc(err61_diff)
  )
#desc(err52))



#count number of errors for each holding (exclude parish and holding, first two columns)

main_validations$total_errors_per_case <-
  rowSums(main_validations[names(main_validations) %in% relevant_errors], na.rm =
            TRUE)
migrant_errors$total_errors_per_case <-
  rowSums(migrant_errors[names(migrant_errors) %in% relevant_merrors], na.rm =
            TRUE)

#filter where total error per case !=0
main_validations <-
  main_validations %>% filter(total_errors_per_case != 0)
migrant_errors <-
  migrant_errors %>% filter(total_errors_per_case != 0)

#filter to select cases where err60 is the only error present
legal_only_error <-
  main_validations %>% filter(err60 == 1 & total_errors_per_case == 1)


#anti-join to remove cases where err60 is the only error present from total error cases
main_validations <-
  anti_join(main_validations, legal_only_error, by = c("parish", "holding"))

#non-priority errors (i.e. holdings with errors not on priority list)
#will already include cases where err60 is only error present
non_priority_full <-
  all_JAC_form %>% select(parish, holding, any_of(all_validations)) %>%  filter(if_any(starts_with("err"), ~ . !=
                                                                                         0))
non_priority_full <-
  anti_join(non_priority_full, main_validations,
            by = c("parish", "holding"))
non_priority_full$total_errors_per_case <-
  rowSums(non_priority_full[grep("err", names(non_priority_full))], na.rm =
            TRUE)
non_priority_full <-
  non_priority_full %>% filter(total_errors_per_case != 0)
non_priority_full <- remove_zero(non_priority_full)

#single dataframe of holdings with validation errors
holding_list <- remove_zero(main_validations)


#unlist and save as dataframe (preserve error names)
holding_list <- as.data.frame(lapply(holding_list, as.numeric))
non_priority_full <-
  as.data.frame(lapply(non_priority_full, as.numeric))

#adding priority and non-priority error counts
priority_error_count <-
  c("total_cases_with_priority_errors", nrow(main_validations))
non_priority_full_error_count <-
  c("total_cases_with_non_priority_full_errors",
    nrow(non_priority_full))


#TRIM List for  Ops-------------------------------------------------------------------------------------------
#trim down errors - remove errors that analysts  can correct without Ops intervention
trim_list <-
  c(
    "err13",
    "err15",
    "err57",
    "err59",
    "err60",
    "err13",
    "err29",
    "err3",
    "err5",
    "err6",
    "err7",
    "err8",
    "err9",
    "err13",
    "err32",
    "err17"
  )

non_priority_trim <-
  non_priority_full %>%  select(-any_of(trim_list),-total_errors_per_case)

#calculate new total errors per case, after trimming error list
non_priority_trim$total_errors_per_case <-
  rowSums(non_priority_trim[grep("err", names(non_priority_trim))], na.rm =
            TRUE)
non_priority_trim <-
  non_priority_trim %>% filter(total_errors_per_case != 0)
non_priority_trim <- remove_zero(non_priority_trim)


non_priority_trim_error_count <-
  c("total_cases_with_non_priority_errors_trim",
    nrow(non_priority_trim))


#priority validation summary
priority_main_validations_summary <-
  holding_list %>% ungroup %>% select(-c(parish, holding, contains(c("diff", "total")))) %>%  summarize(across(everything(), sum, na.rm = TRUE))
priority_main_validations_summary <-
  cbind(
    priority_main_validations_summary,
    total_cases_with_errors = rowSums(priority_main_validations_summary[names(priority_main_validations_summary) %in% all_validations])
  )
priority_main_validations_summary <-
  priority_main_validations_summary %>% pivot_longer(cols = everything(),
                                                     names_to = "error",
                                                     values_to = "count")
priority_main_validations_summary <-
  priority_main_validations_summary %>% filter(count != 0)

#non_priority-full validation summary
non_priority_main_validations_summary <-
  non_priority_full %>% ungroup %>% select(-c(parish, holding, contains(c("diff", "total")))) %>%  summarize(across(everything(), sum, na.rm = TRUE))
non_priority_main_validations_summary <-
  cbind(
    non_priority_main_validations_summary,
    total_cases_with_errors = rowSums(non_priority_main_validations_summary[names(non_priority_main_validations_summary) %in% all_validations])
  )
non_priority_main_validations_summary <-
  non_priority_main_validations_summary %>% pivot_longer(cols = everything(),
                                                         names_to = "error",
                                                         values_to = "count")
non_priority_main_validations_summary <-
  non_priority_main_validations_summary %>% filter(count != 0)

#non_priority trim validation summary
non_priority_trim_validations_summary <-
  non_priority_trim %>% ungroup %>% select(-c(parish, holding, contains(c("diff", "total")))) %>%  summarize(across(everything(), sum, na.rm = TRUE))
non_priority_trim_validations_summary <-
  cbind(
    non_priority_trim_validations_summary,
    total_cases_with_errors = rowSums(non_priority_trim_validations_summary[names(non_priority_trim_validations_summary) %in% all_validations])
  )
non_priority_trim_validations_summary <-
  non_priority_trim_validations_summary %>% pivot_longer(cols = everything(),
                                                         names_to = "error",
                                                         values_to = "count")
non_priority_trim_validations_summary <-
  non_priority_trim_validations_summary %>% filter(count != 0)

prioritised_main_validation_summary <-
  rbind(
    priority_main_validations_summary,
    non_priority_main_validations_summary,
    non_priority_trim_validations_summary
  )
#Save Outputs----------------------------------------------------------------------------------------------

#Uncomment as necessary
# #Save to ADM
#
#submisType summary
write_dataframe_to_db(server=server,
                      database=database,
                      schema=schema,
                      table_name="JAC23_main_validation_summary",
                      dataframe=JAC_validation_error_summary,
                      append_to_existing = FALSE,
                      versioned_table=FALSE,
                      batch_size = 10000)
#
# #prioritised summary
#   write_dataframe_to_db(server=server,
#                         database=database,
#                         schema=schema,
#                         table_name="JAC23_main_validation_prioritised_summary",
#                         dataframe=prioritised_main_validation_summary,
#                         append_to_existing = FALSE,
#                         versioned_table=FALSE,
#                         batch_size = 10000)
#
#main validations
write_dataframe_to_db(
  server = server,
  database = database,
  schema = schema,
  table_name = "JAC23_main_validation_list",
  dataframe = holding_list,
  append_to_existing = FALSE,
  versioned_table = FALSE,
  batch_size = 10000
)


#
#non-priority main validations- trimmed
write_dataframe_to_db(
  server = server,
  database = database,
  schema = schema,
  table_name = "JAC23_main_validation_non_priority_list_trim",
  dataframe = non_priority_trim,
  append_to_existing = FALSE,
  versioned_table = FALSE,
  batch_size = 10000
)



#non-priority main validations-full
write_dataframe_to_db(server=server,
                      database=database,
                      schema=schema,
                      table_name="JAC23_main_validation_non_priority_list_full",
                      dataframe=non_priority_full,
                      append_to_existing = FALSE,
                      versioned_table=FALSE,
                      batch_size = 10000)
#
#
# #migrant checks
#
# #main validations
# write_dataframe_to_db(server=server,
#                       database=database,
#                       schema=schema,
#                       table_name="JAC23_labour_validation_list",
#                       dataframe=migrant_errors,
#                       append_to_existing = FALSE,
#                       versioned_table=FALSE,
#                       batch_size = 10000)
#
#

#
#

#list of dataframes to export as a multi-sheet xlsx
#priority_holdings <-list("error_summary" = JAC_validation_error_summary,  "main_validations" = holding_list, "non-priority_errors" = non_priority, "migrant_worker_checks" = labour_holding_list)


# #export prioritised holding list as xlsx for Ops
# write_xlsx(priority_holdings, paste(output_path, Sys.Date(), "work_item_prioritised.xlsx"))
