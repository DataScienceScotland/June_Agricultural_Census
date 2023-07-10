#Tables for context error checks------------------------------------------
#can use June items from Item categories file or, if using with Rmd items appear in "context item vars" section 
#NB
#Excluding cattle data out for now as not in JAC23 dataset at this point. Will add back in if ScotEid Data us added to dataset in step A 

# Before import -----------------------------------------------------------


# Clear environment prior
rm(list=ls())



#yr = this year 20xx
yr <- 23
yr1 <- yr-2 
yr2 <-  yr-3
yr3 <-  yr-4


# Datashare file path for import and export

output_path <- "//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Codeconversion_2023/"


Code_directory <-
  ("//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Codeconversion_2023")

# ADM schema for export

server <- "s0196a\\ADM"
database <- "RuralAndEnvironmentalScienceFarmingStatistics"
schema <- "juneagriculturalsurvey2023alpha"


#SAS dataset path 
#sas_agscens_path <- "//s0177a/sasdata1/ags/census/agscens/"
#sas_agstemp_path <- "//s0177a/sasdata1/ags/census/agstemp/"

# Load packages

library(tidyverse)
library(stringr)
library(data.table)
library(haven)
library(RtoSQLServer)

# Load functions and item lists--------------------------------------------------------
#Functions

#function to keep variables relevant to error
keep_perr_vars <- function(x){
  perr_x <<- errors %>% select(parish, holding, survtype, any_of(x))    
  name_x <<- sub("\\_.*", "", substitute(x))
}

#function to remove columns where error = 0
remove_zero <- function(x){select(x, where(~any(.!= 0)))}


#relevant item list -------------------------------------------------------------------

#Section 1
area_rent <- "item20026"
area_rent_old <- "item7"
area_own <- "item11"
total_area <- "item12"


#Section 3
seed_potatoes <- "item24"
ware_potatoes <- "item2320"
soft_fruits <- "item37"

glass_strawb_tot <- "item2556"
glass_rasp_tot <- "item2557"
glass_blue_tot <- "item2836"
glass_bed_pot_plant_tot <- "item1711"
glass_hardy_nurse_tot <- "item1943"
#Walk in plastic structure greenhouse (Total open soil area)
tot_open_plastic <- "item85"
#Glass clad structure greenhouses (Total solid floor area)
tot_solid_glass <- "item86"
glass_unused_tot <- "item6001"


total_crops_fallow <- "item40"
#Grass under 5 years old (seeded or re-seeded in the last 5 years)
fland_lte5 <- "item2469"
fland_gt5 <- "item2470"

#fallow land (fland_tot) is item39
fland_tot <- c("item2469", "item2470")
grass_lt5 <- "item2321"
grass_gte5 <- "item2322"
rough_graze <- "item47"
other_land <- "item49"
total_land <-  "item50"

#Section 4
total_pig <- "item157"

#Section 5
ewes <- "item139"
rams <- "item140"
breed_other_sheep <- "item141"
no_breed_other_sheep <- "item143"
lambs <- "item144"
total_sheep <- "item145"

#Section6
layer_chicks <- "item160"
table_chicks <- "item162"
cocks <- "item163"
total_poultry <- "item170"

#Section 7
deer <- "item94"

#Section 11
#Note that the definition of 2511 has changed. For comparability, pre-2023 2511 = new2511+new2536
#i.e. checks for 2023 checks against 2023 (item2511 + item2536) will be performed against prev years item2511 
labour_non_EU_migrant <- "item2511"
labour_EU_migrant <- "item2536"
labour_migrant <- c("item2511", "item2536")

#named list of items
context_items <- list(area_rent = area_rent,
                      area_rent_old = area_rent_old, 
                      area_own = area_own, 
                      total_area=total_area, 
                      total_crops_fallow= total_crops_fallow, 
                      total_pig= total_pig,
                      total_poultry = total_poultry, 
                      grass_lt5 = grass_lt5,
                      grass_gte5 = grass_gte5,
                      total_sheep = total_sheep, 
                      ewes = ewes,
                      rams = rams,
                      breed_other_sheep = breed_other_sheep,
                      no_breed_other_sheep = no_breed_other_sheep, 
                      lambs = lambs,
                      rough_graze =rough_graze,
                      other_land =other_land,
                      soft_fruits = soft_fruits,
                      fland_lte5=fland_lte5, 
                      fland_gt5=fland_gt5,
                      fland_tot = fland_tot,
                      glass_strawb_tot = glass_strawb_tot,
                      glass_rasp_tot = glass_rasp_tot,
                      glass_blue_tot = glass_blue_tot,
                      glass_bed_pot_plant_tot = glass_bed_pot_plant_tot,
                      glass_hardy_nurse_tot = glass_hardy_nurse_tot,
                      tot_open_plastic=tot_open_plastic,
                      tot_solid_glass=tot_solid_glass,
                      glass_unused_tot= glass_unused_tot,
                      seed_potatoes=seed_potatoes, 
                      ware_potatoes=ware_potatoes, 
                      layer_chicks=layer_chicks,
                      table_chicks=table_chicks,
                      cocks=cocks, 
                      deer = deer,
                      total_land = total_land,
                      labour_non_EU_migrant=labour_non_EU_migrant,
                      labour_EU_migrant=labour_EU_migrant,
                      labour_migrant = labour_migrant)



#add "o" prefix to prev yr items 
context_items_yr1 <- lapply(context_items, function (x) paste("o", x, sep = "") )
names(context_items_yr1) <-lapply(names(context_items), function (x) paste("o", x, sep = "") ) 

#add "vo" prefix to second to last (prev_2yr) year's items 
context_items_yr2 <- lapply(context_items, function (x) paste("vo", x, sep = "") )
names(context_items_yr2) <-lapply(names(context_items), function (x) paste("vo", x, sep = "") ) 

#add list items to environment
mapply(assign, names(context_items_yr1), context_items_yr1, MoreArgs=list(envir = globalenv()))
mapply(assign, names(context_items_yr2), context_items_yr2, MoreArgs=list(envir = globalenv()))

#error variables to display in tables
perr1_vars<-c("perr1", "checkperr1",  "tillage", "otillage", "votillage",
              total_area, ototal_area, vototal_area,
              "grass", "ograss", "vograss",
              "othgrass", "oothgrass", "voothgrass",
              rough_graze, orough_graze, vorough_graze) 
perr2_vars <- c("perr2", total_pig, ototal_pig, vototal_pig)
perr3_vars <- c("perr3", total_poultry, ototal_poultry, vototal_poultry)
perr4_vars <- c("perr4", "cts312", "octs312", "vocts312")
perr5_vars <- c("perr5", "dairy", "odairy", "vodairy")
perr6_vars <- c("perr6","beef", "obeef", "vobeef")
perr7_vars <- c("perr7", total_sheep, ototal_sheep, vototal_sheep)
perr9_vars <- c("perr9",  soft_fruits, osoft_fruits, vosoft_fruits)
perr10_vars <- c("perr10", "glass", "oglass", "voglass")
perr11_vars <- c("perr11", "male", "omale", "vomale")
perr12_vars <- c("perr12", "female", "ofemale", "vofemale")
perr13_vars <- c("perr13", "ware", "oware", "voware", seed_potatoes, oseed_potatoes, voseed_potatoes)
perr14_vars <- c("perr14", deer, odeer, vodeer)
perr15_vars <- c("perr15", total_land, ototal_land, vototal_land)
perr16_vars <- c("perr16", rough_graze, orough_graze, vorough_graze)
perr17_vars <- c("perr17", labour_migrant, olabour_EU_migrant, volabour_EU_migrant)

##Context error descriptions
perr1_desc <- "perr1_Old grass increased to more than all grass last year"
perr2_desc <- "perr2_Large change in pigs"
perr3_desc <- "perr3_Suspicious change in poultry numbers"
perr4_desc <- "perr4_Large change in total cattle"
perr5_desc <- "perr5_Introduction of dairy to beef holding"
perr6_desc <- "perr6_Introduction of beef to dairy holding"
perr7_desc <- "perr7_Large change in total sheep numbers"
perr9_desc <- "perr9_Large change in soft fruit area"
perr10_desc <- "perr10_Large change in glasshouse area"
perr11_desc <- "perr11_Introduction of male cattle to holding with only female cattle (excluding bulls for service)"
perr12_desc <- "perr12_Introduction of female cattle not for breeding to holding with only male cattle"
perr13_desc <- "perr13_Switch from ware to seed potatoes"
perr14_desc <- "perr14_Large shift in deer numbers or movement from zero or to zero"
perr15_desc <- "perr15_Large change in total land area"
perr16_desc <- "perr16_Large change in rough grazing area"
perr17_desc <- "perr17_Large change in migrant labour"




# SAS Import data -------------------------------------------------------------



#SAS datasets for now, will be saved on ADM server so edit accordingly 

# #This year's data
# june_context <- read_sas(paste0(sas_agstemp_path, "combined_data", yr, ".sas7bdat")) %>% select (parish, holding,  submisType, all_of(context_June_items), other_data, land_data ) %>% group_by(parish, holding) 
# #assign(paste0("june_context", yr), june_context)
# #rm(june_context)
# 
# 
# #last year's data 
# prev_yr <- read_sas(paste0(sas_agscens_path, "june20", yr1, ".sas7bdat")) %>% select(parish, holding, ags_madeup, saf_madeup, all_of(context_June_items_yr1)) %>% 
#   rename_with(.fn = function(.x){paste0("o", .x)},
#               .cols= -c(parish, holding))%>% 
#   group_by(parish, holding) %>% 
#   mutate(fland_tot = ofland_lte5  + ofland_gt5)
# 
# # last 2 year's data 
# prev_2yr <- read_sas(paste0(sas_agscens_path, "june20", yr2, ".sas7bdat")) %>% select(parish, holding, ags_madeup, saf_madeup, all_of(context_June_items_yr2)) %>% 
#   rename_with(.fn = function(.x){paste0("vo", .x)},
#               .cols= -c(parish, holding))%>% 
#   group_by(parish, holding) 

# Import ------------------------------------------------------------------


#This years data - 2023
combined_JAC <- read_table_from_db(server=server, 
                                   database=database, 
                                   schema=schema, 
                                   table_name="combined_data_2023")


JAC23 <- combined_JAC %>% group_by(parish, holding) %>% 
  select (parish, holding,  survtype, submisType, any_of(as.character(context_items)), other_data, land_data )


#previous year's data (2021)

prev_yr <-  read_table_from_db(server=server, 
                               database=database, 
                               schema=schema, 
                               table_name="jac_2021_update")


prev_yr<- prev_yr %>% group_by(parish, holding) %>% 
  select (parish, holding, survtype, ags_madeup, saf_madeup, #submisType,
          any_of(as.character(context_items)), other_data, land_data )

prev_yr <- prev_yr %>%   
  mutate(fland_tot = (get(fland_lte5) +get(fland_gt5))) %>% 
  rename_with(.fn = function(.x){paste0("o", .x)},
              .cols= -c(parish, holding))%>% 
  group_by(parish, holding)


#second to last year data (2020)

prev_2yr <-  read_table_from_db(server=server, 
                               database=database, 
                               schema=schema, 
                               table_name="jac_2020_update")

prev_2yr<- prev_2yr %>% group_by(parish, holding) %>% 
  select (parish, holding, survtype, ags_madeup, saf_madeup, #submisType,
          any_of(as.character(context_items)), other_data, land_data )

prev_2yr <- prev_2yr %>%   
  mutate(fland_tot = (get(fland_lte5) +get(fland_gt5))) %>% 
  rename_with(.fn = function(.x){paste0("vo", .x)},
              .cols= -c(parish, holding))%>% 
  group_by(parish, holding)




#merge datasets
context_errors <- inner_join(prev_yr, JAC23, by = c("parish", "holding"))
context_errors<- right_join(prev_2yr, context_errors, by = c("parish", "holding"))

all_context_errors <- context_errors %>% 
  mutate(tillage=sum(get(total_crops_fallow),get(grass_lt5)),
         otillage=sum(get(ototal_crops_fallow),get(ograss_lt5)),
         votillage=sum(get(vototal_crops_fallow),get(vograss_lt5)),
         grass=get(grass_lt5),
         ograss=get(ograss_lt5),
         vograss=get(vograss_lt5),
         othgrass=get(grass_gte5),
         oothgrass=get(ograss_gte5),
         voothgrass=get(vograss_gte5), 
         #comment out cattle data -see note at start
         # cts303 = as.numeric(""),
         # cts304= as.numeric(""),
         # cts305= as.numeric(""),
         # cts306= as.numeric(""),
         # cts307= as.numeric(""),
         # cts308= as.numeric(""),
         # octs303= as.numeric(""),
         # octs304= as.numeric(""),
         # octs305= as.numeric(""),
         # octs306= as.numeric(""),
         # octs307= as.numeric(""),
         # octs308= as.numeric(""),
         # vocts303= as.numeric(""),
         # vocts304= as.numeric(""),
         # vocts305= as.numeric(""),
         # vocts306= as.numeric(""),
         # vocts307= as.numeric(""),
         # vocts308= as.numeric(""),
         # dairy=sum(cts304,cts306,cts308),
         # odairy=sum(octs304,octs306,octs308),
         # vodairy=sum(vocts304,vocts306,vocts308),
         # beef=sum(cts303,cts305,cts307),
         # obeef=sum(octs303,octs305,octs307),
         # vobeef=sum(vocts303,vocts305,vocts307),
         #item87 (protected crops - tomatoes total solid + glass) is no longer collected from form 2023 onwards- SAF dataset only collects item2707 (protected crops tomatoes solid floor))
         #item87 =as.numeric(""),
         #oitem87 = as.numeric(""),
         #voitem87 = as.numeric(""),
         item2556 =as.numeric(""),
         oitem2556 = as.numeric(""),
         voitem2556 = as.numeric(""),
         item2557 =as.numeric(""),
         oitem2557 = as.numeric(""),
         voitem2557 = as.numeric(""),
         item2836 = as.numeric(""),
         oitem2836 = as.numeric(""),
         #item2036 - glasshouse: Protected crops area of which is other fruit is not in 2023 dataset
         # item2036 =as.numeric(""),
         # oitem2036 = as.numeric(""),
         # voitem2036 = as.numeric(""), 
         #item2037 - glasshouse: protected crops area of which is vegetables is not in 2023 dataset
         # item2037 =as.numeric(""),
         # oitem2037 = as.numeric(""),
         # voitem2037 = as.numeric(""),
         item1711 =as.numeric(""),
         oitem1711 = as.numeric(""),
         voitem1711 = as.numeric(""),
         item1943 =as.numeric(""),
         oitem1943 = as.numeric(""),
         voitem1943 = as.numeric(""),
         #item6000 - glasshouse protected crops area of which is blackcurrants not in 2023 dataset
         #item6000 =as.numeric(""),
         item6001 =as.numeric(""),
         glass=sum(#item87, 
           get(glass_strawb_tot), get(glass_rasp_tot), get(glass_blue_tot), 
           #item2036,item2037,
           get(glass_bed_pot_plant_tot), get(glass_hardy_nurse_tot), #item6000, 
           get(glass_unused_tot)), 
         oglass=sum(#oitem87,
           get(oglass_strawb_tot), get(oglass_rasp_tot), get(oglass_blue_tot), 
           #oitem2036, oitem2037, 
           get(oglass_bed_pot_plant_tot), get(oglass_hardy_nurse_tot), get(oglass_blue_tot)),
         voglass=sum(#voitem87,
           get(voglass_strawb_tot), get(voglass_rasp_tot), get(voglass_blue_tot), #voitem2036, voitem2037,
           get(voglass_bed_pot_plant_tot), get(voglass_hardy_nurse_tot)),
         ware=get(ware_potatoes),
         oware=get(oware_potatoes),
         voware=get(voware_potatoes),
         rgr = get(rough_graze),
         orgr = get(orough_graze),
         vorgr = get(vorough_graze),
         land = get(total_land),
         oland = get(ototal_land),
         voland = get(vototal_land),
#As noted 2023 and onwards, need to sum labour_EU_migrant and labour_non_EU_migrant to compare to prev years
         migrant = sum(get(labour_EU_migrant),get(labour_non_EU_migrant)),
         omigrant = get(olabour_non_EU_migrant),
         vomigrant = get(volabour_non_EU_migrant),
  )


#add context error checks

  

all_context_errors<- all_context_errors %>% 
  mutate(
  #Land check - If total area has gone up or down by 10% and there has been an increase of grass greater than 5 years in the last year that does not account for the total of last year's rough grazings, fallow, other land and the difference in total land use area change.
    checkperr1 = 0.9*get(ototal_area),
    perr1 = case_when(0.9*get(oarea_own)< get(total_area) & 
                         get(total_area)<1.1*get(ototal_area) & 
                         othgrass - oothgrass > ograss+ get(orough_graze)+ get(fland_tot) +get(oother_land) + 20 + max(get(total_area)-get(ototal_area), 0)~ 1,
                      TRUE ~ 0),
    #Pigs check - if the total number of pigs is greater than 5 times or less than a fifth of last year's pigs and the land area has not as much as doubled
    perr2 = case_when((get(total_pig) > 5.*get(ototal_pig) & get(total_area) < 2*get(ototal_area))  | 
                         (get(total_pig) < 0.2*get(ototal_pig) & get(total_area) > 0.5*get(ototal_area)) &
                          ((get(total_pig)-get(ototal_pig)) > 10000 | (get(ototal_pig)-get(total_pig)) > 1000) &
                          (get(total_pig) > 5.*get(vototal_pig) | get(total_pig) < 0.2*get(vototal_pig)) &
                          ((get(total_pig)-get(vototal_pig)) > 10000 | get(vototal_pig)-get(total_pig) > 1000) &
                          (get(ototal_pig)-get(total_pig) > 1.5*(get(vototal_pig)-get(ototal_pig))& 
                        get(ototal_pig)-get(total_pig) > 0 |get(ototal_pig)-get(total_pig) < 1.5*(get(vototal_pig)-get(ototal_pig)) & 
                        get(ototal_pig)-get(total_pig)) < 0  ~ 1,
                      TRUE ~ 0),
      
    
    #Poultry check - if total poultry is greater than 8 times or less than an eighth last year's poultry and the land area has not as much as doubled----------------------------
 
    perr3 = case_when ((get(total_poultry) > 8.*get(ototal_poultry) & get(total_area) < 2*get(ototal_area)) |
                         (get(total_poultry) < 0.125*get(ototal_poultry) & get(total_area) > 0.5*get(ototal_area)) & 
                         (get(total_poultry)-get(ototal_poultry) > 150000| (get(ototal_poultry)-get(total_poultry)) > 100000) & 
                         (get(total_poultry) > 8.*get(vototal_poultry) | get(total_poultry) < 0.125*get(vototal_poultry)) &
                         (get(total_poultry)-get(vototal_poultry)) > 150000 | (get(vototal_poultry)-get(total_poultry) > 100000) &
                         (get(ototal_poultry)-get(total_poultry) > 1.5*(get(vototal_poultry)-get(ototal_poultry)) & get(ototal_poultry)-get(total_poultry) > 0|
                           get(ototal_poultry)-get(total_poultry) < 1.5*(get(vototal_poultry)-get(ototal_poultry)) & get(ototal_poultry)-get(total_poultry) < 0) ~1,
    TRUE~0),
	 
	#Sheep check ------------------------------------------------------------------------------------------------------------------------------------------
perr7= case_when((get(total_sheep) > 10.*get(ototal_sheep) & get(total_area) < 2*get(ototal_area)) |
                    (get(total_sheep) < 0.1*get(ototal_sheep) & get(total_area) > 0.5*get(ototal_area)) &
                   abs(get(total_sheep)-get(ototal_sheep)) > 5000 & 
                   (get(total_sheep) > 10.*get(vototal_sheep) | get(total_sheep) < 0.1*get(vototal_sheep)) &
                   abs(get(total_sheep)-get(vototal_sheep)) > 5000 &
                   (get(ototal_sheep)-get(total_sheep) > 1.5*(get(vototal_sheep)-get(ototal_sheep)) & get(ototal_sheep)-get(total_sheep) > 0 |
                      get(ototal_sheep)-get(total_sheep) < 1.5*(get(vototal_sheep)-get(ototal_sheep)) & get(ototal_sheep)-get(total_sheep) < 0) ~ 1, 
                 TRUE~0),
 

	 #Soft fruit check--------------------------------------------------------------------------------------------------------
perr9 =  case_when((get(soft_fruits)  > 3.*get(osoft_fruits) | get(soft_fruits)  < 0.3*get(osoft_fruits)) & 
                     ((get(soft_fruits) -get(osoft_fruits)) > 10 | (get(osoft_fruits) -get(soft_fruits)) > 1)&
                     (get(soft_fruits)  > 3.*get(vosoft_fruits) | get(soft_fruits)  < 0.3*get(vosoft_fruits)) & 
                     ((get(soft_fruits) -get(vosoft_fruits)) > 10 | (get(vosoft_fruits) -get(soft_fruits)) > 1) & 
  (get(osoft_fruits) -get(soft_fruits)  > 1.5*(get(vosoft_fruits) -get(osoft_fruits)) & get(osoft_fruits) -get(soft_fruits)  > 0 | 
     get(osoft_fruits) -get(soft_fruits)  < 1.5*(get(vosoft_fruits) -get(osoft_fruits)) & get(osoft_fruits) -get(soft_fruits) < 0) & 
  (get(soft_fruits)  > 50 | get(osoft_fruits)  > 3)~ 1,
	TRUE ~ 0),

	#Glass houses check ------------------------------------------------------------------------------------------------
perr10 = case_when((glass > 0.1*oglass | glass < 0.1*oglass) &
    abs(glass-oglass) > 0.02 & (oglass > 0 | glass > 50) &
    (glass > 200*voglass | glass < 0.5*voglass) &
    abs(glass-voglass) > 0.02 &
    (oglass-glass > 1.5*(voglass-oglass) & oglass-glass > 0 |oglass-glass < 1.5*(voglass-oglass) & oglass-glass < 0) ~1,
    TRUE~ 0),
	
	#Land check------------------------------------------------------------------------------------------------------------
#NA check here: SAS includes 3 entries where item12 is missing, whilst R does not....
perr13 = case_when(get(total_area)< 1.1*get(ototal_area)  && get(oseed_potatoes) == 0 & get(voseed_potatoes)  == 0 & get(seed_potatoes) > 10
   & ware == 0 & oware > 5 & voware > 5 ~1,
   TRUE~0),

#account for Inf when dividing by 0 i.e if odeer = 0 then change to 1
deer_ratio = case_when(get(odeer) == 0 ~ 0,
                         TRUE ~ (get(deer)/get(odeer))),

#deer_ratio = deer/odeer,

#is it oags_madeup? SAS code says ags_madeup but this doesn't exist. commented out to match SAS output...
perr14 = case_when((get(deer)>=50| get(odeer) >= 50) && ((deer_ratio >1.8) |(deer_ratio<= 0.5 &deer_ratio>0)) ~1,
                  # oags_madeup == 0 & deer<1 & odeer>0 ~1,
                   #oags_madeup == 0 & deer>0 &odeer<1 ~1,
                            TRUE ~0)
)
  


all_context_errors <- all_context_errors %>% 
  mutate(perr15 = case_when(survtype == "Non-SAF" & (get(total_land)>= 2*get(ototal_land) |get(total_land)<0.5*get(ototal_land)) &
                              (get(total_land)>1000 | get(ototal_land) > 1000) ~1,
                            TRUE ~0),
         perr16 = case_when(survtype == "Non-SAF" & (get(rough_graze)>= 2*get(orough_graze) |get(rough_graze)<0.5*get(orough_graze)) &
                              (get(rough_graze)>1000 | get(orough_graze) > 1000) ~1,
                            TRUE ~0)
           
         )

  
  
##----perr17 doesn't appear to be computed in SAS code...therefore not mutated here.          


#Total errors in dataset-----------------------------------------------------------------------------------
errors <- all_context_errors %>% filter((sum(perr2, perr3, perr7, perr10, perr13, perr14)>0 & survtype == "Non-SAF")| 
                                          (sum(perr2, perr3, perr7, perr10, perr14) > 0 & survtype == "SAF"))









##----perr 1----------------------------------------------------------------------------------------------
##@knitr perr1

keep_perr_vars <- function(x){
    perr_x <<- errors %>% select(parish, holding, survtype, any_of(x))    
  name_x <<- sub("\\_.*", "", substitute(x))
}

keep_perr_vars(perr1_vars)
assign(name_x, perr_x)
perr1 <-perr1 %>%  mutate(error=ifelse(perr1== 1, "Error", "No Error"),
                          difference = (tillage-otillage))
perr1 <- perr1 %>% filter(error == "Error")
                          
                          
##----perr 2----------------------------------------------------------------------------------
keep_perr_vars(perr2_vars)
assign(name_x, perr_x)



perr2 <-perr2 %>%  mutate(error=ifelse(perr2== 1, "Error", "No Error"),
                           difference = ifelse(get(total_pig)-get(ototal_pig) < 0, -0.5*(get(total_pig)-get(ototal_pig)), get(total_pig)-get(ototal_pig))
)

perr2 <- perr2 %>% filter(error == "Error")

##----perr 3----------------------------------------------------------------------------------
keep_perr_vars(perr3_vars)
assign(name_x, perr_x)


perr3 <-perr3 %>%  mutate(error=ifelse(perr3== 1, "Error", "No Error"),
                          difference = ifelse(get(total_poultry) -get(ototal_poultry) < 0, -0.5*get(total_poultry)-get(ototal_poultry), get(total_poultry)-get(ototal_poultry))
)


perr3 <- perr3 %>% filter(error == "Error")

# 
# 
# ###----perr 4 [CTS312 ISN'T IN PE ALL SAS OUTPUT....NOT SURE WHY IT IS IN CODE]----------------------------------------------------------------------------------
# keep_perr_vars(perr4_vars)
# assign(name_x, perr_x)
# 
# 
# perr4 <-perr4 %>%  mutate(error=ifelse(perr4== 1, "Error", "No Error"),
#                           difference = ifelse(cts312 -octs312 < 0, -0.5*(cts312-octs312), cts312-octs312)
# )
# 




###----perr 5 NOT IN CODE EITHER----------------------------------------------------------------------------------
# keep_perr_vars(perr5_vars)
# assign(name_x, perr_x)
# 
# 
# perr5 <-perr5 %>%  mutate(error=ifelse(perr5== 1, "Error", "No Error")
#                          )
# 


###----perr 6 NOT IN CODE EITHER----------------------------------------------------------------------------------
# keep_perr_vars(perr6_vars)
# assign(name_x, perr_x)
# 
# 
# perr6 <-perr6 %>%  mutate(error=ifelse(perr6== 1, "Error", "No Error")
# )


##----perr7------------------------------------------------------------------------------------------------
keep_perr_vars(perr7_vars)
assign(name_x, perr_x)


perr7 <-perr7 %>%  mutate(error=ifelse(perr7== 1, "Error", "No Error"),
                          difference = ifelse(get(total_sheep) -get(ototal_sheep) < 0, -0.5*(get(total_sheep)-get(ototal_sheep)), get(total_sheep)-get(ototal_sheep))
)

perr7 <- perr7 %>% filter(error == "Error")

##----perr9------------------------------------------------------------------------------------------------
keep_perr_vars(perr9_vars)
assign(name_x, perr_x)


perr9 <-perr9 %>%  mutate(error=ifelse(perr9== 1, "Error", "No Error"),
                          difference = ifelse(get(soft_fruits)  -get(osoft_fruits)  < 0, -0.5*(get(soft_fruits) -get(osoft_fruits) ), get(soft_fruits) -get(osoft_fruits))
)


perr9 <- perr9 %>% filter(error == "Error")
##----perr10------------------------------------------------------------------------------------------------
keep_perr_vars(perr10_vars)
assign(name_x, perr_x)


perr10 <-perr10 %>%  mutate(error=ifelse(perr10== 1, "Error", "No Error"),
                          difference = ifelse(glass -oglass < 0, -0.5*(glass-oglass), glass-oglass)
)

perr10 <- perr10 %>% filter(error == "Error")

# ##----perr11 NOT IN CODE EITHER------------------------------------------------------------------------------------------------
# keep_perr_vars(perr11_vars)
# assign(name_x, perr_x)
# 
# 
# perr11 <-perr11 %>%  mutate(error=ifelse(perr11== 1, "Error", "No Error"))
# 



# ##----perr12  NOT IN CODE EITHER------------------------------------------------------------------------------------------------
# keep_perr_vars(perr12_vars)
# assign(name_x, perr_x)
# 
# 
# perr12 <-perr12 %>%  mutate(error=ifelse(perr12== 1, "Error", "No Error"))


##----perr13------------------------------------------------------------------------------------------------
keep_perr_vars(perr13_vars)
assign(name_x, perr_x)



perr13 <-perr13 %>%  mutate(error=ifelse(perr13== 1, "Error", "No Error")) 
perr13 <- perr13 %>% filter(error== "Error")


##----perr14------------------------------------------------------------------------------------------------
keep_perr_vars(perr14_vars)
assign(name_x, perr_x)


perr14 <-perr14 %>%  mutate(error=ifelse(perr14== 1, "Error", "No Error"))
perr14 <- perr14 %>% filter(error == "Error")
##----perr15------------------------------------------------------------------------------------------------
keep_perr_vars(perr15_vars)
assign(name_x, perr_x)


perr15 <-perr15 %>%  mutate(error=ifelse(perr15== 1, "Error", "No Error"))
perr15 <- perr15 %>% filter(error == "Error")

##----perr16------------------------------------------------------------------------------------------------
keep_perr_vars(perr16_vars)
assign(name_x, perr_x)


perr16 <-perr16 %>%  mutate(error=ifelse(perr16== 1, "Error", "No Error"))
perr16 <- perr16 %>% filter(error == "Error")

# ##----perr17------------------------------------------------------------------------------------------------
# keep_perr_vars(perr17_vars)
# assign(name_x, perr_x)
# 
# 
# perr17 <-perr17 %>%  mutate(error=ifelse(perr17== 1, "Error", "No Error"))
# 
# 
#

##----Context Error-Summary---------------------------------------------------------------------------------------------
error_count <- all_context_errors %>% ungroup() %>% 
  select(survtype, starts_with("perr")) %>% group_by(survtype)

  error_count_n <- error_count %>% summarise(count = n())
  
#to fix. For now need to manually type perr variables
error_agg <- aggregate(cbind(perr1, perr2, perr3, perr7, perr9, perr10, perr13, perr14, perr15, perr16) ~ survtype, data = error_count, FUN = sum)

context_error_summary <- full_join(error_count_n, error_agg, by = "survtype")
  
##----Output for Ops--------------------------------------------------------------------------------------------

relevant_errors <- c("perr1", "perr2", "perr3", "perr4", "perr5", "perr6",
                     "perr7", "perr9", "perr10", "perr11", "perr12",
                     "perr13", "perr14", "perr15", "perr16") 


#count number of errors for each holding (exclude parish and holding, first two columns)
errors$total_errors_per_case <- rowSums(errors[names(errors) %in% relevant_errors])

#filter where total error per case !=0 
errors<- errors %>% filter(total_errors_per_case !=0)

#select relevant columns
error_cases <- errors %>% select(parish, holding, survtype, starts_with("perr"), total_errors_per_case)                                      



#create list of validations vector character
all_context_desc <- paste(relevant_errors, "desc", sep="_")

#create list
all_context_desc <-as.list(all_context_desc)  

#change name of each list to error names
names(all_context_desc) <- relevant_errors

all_context_desc <-  lapply(all_context_desc, function(x)get(x))
#single dataframe of errors and error descriptions
all_context_desc <- bind_rows(all_context_desc)

#change value of list element to description of error


#create list of dataframe of errors for each holding
holding_list <- split(error_cases, seq(nrow(error_cases)))


#remove zero columns from each dataframe in holding_list
holding_list <- lapply(holding_list, remove_zero)


#single dataframe of holdings with validation errors
holding_list <- bind_rows(holding_list) 
holding_list <- remove_zero(holding_list)
holding_list <- holding_list %>% relocate(total_errors_per_case, .after = last_col())


#change error column headings to error + description
colnames(holding_list)[colnames(holding_list) %in% colnames(all_context_desc)] <-  all_context_desc %>% select(any_of(names(holding_list)))

#validation summary:change error rownames to error decriptions
colnames(context_error_summary)[colnames(context_error_summary)%in%colnames(all_context_desc)] <- all_context_desc %>% select(any_of(names(context_error_summary)))

#list of dataframes to export as a multi-sheet xlsx
priority_holdings <-list("error_summary" = context_error_summary,  "context_errors" = holding_list)

#export prioritised holding list as xlsx for Ops
write_xlsx(priority_holdings, paste(output_path, Sys.Date(), "context_errors.xlsx"))

