#Tables for punch error checks------------------------------------------
#uses June items from Item categories file

# Before import -----------------------------------------------------------


# Clear environment prior
rm(list=ls())

###check if this has been updated for this year!!
source("Punch error checks/Punch-error-checks_files/Item categories.R")


#yr = this year 20xx
yr <- 21
yr1 <- yr-1 
yr2 <-  yr-2
yr3 <-  yr-3


# Datashare file path for import and export

Code_directory <-
  ("//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Codeconversion_2023")

# ADM schema for export

server <- "s0196a\\ADM"
database <- "RuralAndEnvironmentalScienceFarmingStatistics"
schema <- "juneagriculturalsurvey2023alpha"


#SAS dataset path 
sas_agscens_path <- "//s0177a/sasdata1/ags/census/agscens/"
sas_agstemp_path <- "//s0177a/sasdata1/ags/census/agstemp/"

# Load packages

library(tidyverse)
library(stringr)
library(data.table)
library(haven)

# Load functions and item lists




# Import data -------------------------------------------------------------

#SAS datasets for now, will be saved on ADM server so edit accordingly 

#This year's data
june_punch <- read_sas(paste0(sas_agstemp_path, "combined_data", yr, ".sas7bdat")) %>% select (parish, holding,  survtype, all_of(punch_June_items), other_data, land_data ) %>% group_by(parish, holding) 
#assign(paste0("june_punch", yr), june_punch)
#rm(june_punch)


#last year's data 
prev_yr <- read_sas(paste0(sas_agscens_path, "june20", yr1, ".sas7bdat")) %>% select(parish, holding, ags_madeup, saf_madeup, all_of(punch_June_items_yr1)) %>% 
  rename_with(.fn = function(.x){paste0("o", .x)},
              .cols= -c(parish, holding))%>% 
  group_by(parish, holding) %>% 
  mutate(oitem39 = oitem2469 + oitem2470)

# last 2 year's data 
prev_2yr <- read_sas(paste0(sas_agscens_path, "june20", yr2, ".sas7bdat")) %>% select(parish, holding, ags_madeup, saf_madeup, all_of(punch_June_items_yr2)) %>% 
  rename_with(.fn = function(.x){paste0("vo", .x)},
              .cols= -c(parish, holding))%>% 
  group_by(parish, holding) 


#merge datasets


punch_errors <- inner_join(prev_yr, june_punch, by = c("parish", "holding"))
punch_errors<- right_join(prev_2yr, punch_errors, by = c("parish", "holding"))

all_punch_errors <- punch_errors %>% 
  mutate(tillage=sum(item40,item2321),
         otillage=sum(oitem40,oitem2321),
         votillage=sum(voitem40,voitem2321),
         grass=sum(item2321),
         ograss=sum(oitem2321),
         vograss=sum(voitem2321),
         othgrass=sum(item2322),
         oothgrass=sum(oitem2322),
         voothgrass=sum(voitem2322), 
         cts303 = as.numeric(""),
         cts304= as.numeric(""),
         cts305= as.numeric(""),
         cts306= as.numeric(""),
         cts307= as.numeric(""),
         cts308= as.numeric(""),
         octs303= as.numeric(""),
         octs304= as.numeric(""),
         octs305= as.numeric(""),
         octs306= as.numeric(""),
         octs307= as.numeric(""),
         octs308= as.numeric(""),
         vocts303= as.numeric(""),
         vocts304= as.numeric(""),
         vocts305= as.numeric(""),
         vocts306= as.numeric(""),
         vocts307= as.numeric(""),
         vocts308= as.numeric(""),
         dairy=sum(cts304,cts306,cts308),
         odairy=sum(octs304,octs306,octs308),
         vodairy=sum(vocts304,vocts306,vocts308),
         beef=sum(cts303,cts305,cts307),
         obeef=sum(octs303,octs305,octs307),
         vobeef=sum(vocts303,vocts305,vocts307),
         item87 =as.numeric(""),
         oitem87 = as.numeric(""),
         voitem87 = as.numeric(""),
         item2556 =as.numeric(""),
         oitem2556 = as.numeric(""),
         voitem2556 = as.numeric(""),
         item2557 =as.numeric(""),
         oitem2557 = as.numeric(""),
         voitem2557 = as.numeric(""),
         item2836 = as.numeric(""),
         oitem2836 = as.numeric(""),
         item2036 =as.numeric(""),
         oitem2036 = as.numeric(""),
         voitem2036 = as.numeric(""), 
         item2037 =as.numeric(""),
         oitem2037 = as.numeric(""),
         voitem2037 = as.numeric(""),
         item1711 =as.numeric(""),
         oitem1711 = as.numeric(""),
         voitem1711 = as.numeric(""),
         item1943 =as.numeric(""),
         oitem1943 = as.numeric(""),
         voitem1943 = as.numeric(""),
         item6000 =as.numeric(""),
         item6001 =as.numeric(""),
         glass=sum(item87, item2556, item2557, item2836, item2036, item2037, item1711, item1943, item6000, item6001), 
         oglass=sum(oitem87, oitem2556, oitem2557, oitem2036, oitem2037, oitem1711, oitem1943, oitem2836),
         voglass=sum(voitem87, voitem2556, voitem2557, voitem2036, voitem2037, voitem1711, voitem1943),
         ware=sum(item2320),
         oware=sum(oitem2320),
         voware=sum(voitem2320),
         rgr = sum(item47),
         orgr = sum(oitem47),
         vorgr = sum(voitem47),
         land = sum(item50),
         oland = sum(oitem50),
         voland = sum(voitem50),
         migrant = sum(item2511),
         omigrant = sum(oitem2511),
         vomigrant = sum(voitem2511),
  )


#add punch error checks

  all_punch_errors <- all_punch_errors %>% 
  mutate(
  #Land check - If total area has gone up or down by 10% and there has been an increase of grass greater than 5 years in the last year that does not account for the total of last year's rough grazings, fallow, other land and the difference in total land use area change.
    checkperr1 = 0.9*oitem12,
    perr1 = case_when(0.9*oitem12<item12 & item12<1.1*oitem12 & othgrass - oothgrass > ograss+ oitem47+ oitem39 + oitem49 + 20 + max(item12-oitem12, 0)~ 1,
                      TRUE ~ 0),
    #Pigs check - if the total number of pigs is greater than 5 times or less than a fifth of last year's pigs and the land area has not as much as doubled
    perr2 = case_when(((item157 > 5.*oitem157 & item12 < 2*oitem12) 
                         | (item157 < 0.2*oitem157 & item12 > 0.5*oitem12)) &
                          ((item157-oitem157) > 10000 | (oitem157-item157) > 1000 )&
                          (item157 > 5.*voitem157 | item157 < 0.2*voitem157) &
                          ((item157-voitem157) > 10000 | (voitem157-item157) > 1000 )&
                          (oitem157-item157 > 1.5*(voitem157-oitem157) & oitem157-item157 > 0 |
                             oitem157-item157 < 1.5*(voitem157-oitem157) & oitem157-item157 < 0)  ~ 1,
                      TRUE ~ 0
      
    ),
    #Poulty check - if total poultry is greater than 8 times or less than an eighth last year's poultry and the land area has not as much as doubled
 
    perr3 = case_when (((item170 > 8.*oitem170 & item12 < 2*oitem12) | (item170 < 0.125*oitem170 & item12 > 0.5*oitem12)) &
    ((item170-oitem170) > 150000 | (oitem170-item170) > 100000) &
    (item170 > 8.*voitem170 | item170 < 0.125*voitem170) &
    ((item170-voitem170) > 150000 | (voitem170-item170) > 100000) &
    (oitem170-item170 > 1.5*(voitem170-oitem170) & oitem170-item170 > 0 |
     oitem170-item170 < 1.5*(voitem170-oitem170) & oitem170-item170 < 0) ~1,
    TRUE~0),
	 
	#Sheep check 
perr7= case_when(((item145 > 10.*oitem145 & item12 < 2*oitem12) 	| (item145 < 0.1*oitem145 & item12 > 0.5*oitem12)) &
    abs(item145-oitem145) > 5000 & 
    (item145 > 10.*voitem145 | item145 < 0.1*voitem145) &
    abs(item145-voitem145) > 5000 &
    (oitem145-item145 > 1.5*(voitem145-oitem145) & oitem145-item145 > 0 |
     oitem145-item145 < 1.5*(voitem145-oitem145) & oitem145-item145 < 0)~ 1, 
	TRUE~0),
 

	 #Soft fruit check
perr9 =  case_when((item37 > 3.*oitem37 | item37 < 0.3*oitem37) &
    ((item37-oitem37) > 10 | (oitem37-item37) > 1) & 
    (item37 > 3.*voitem37 | item37 < 0.3*voitem37) &
    ((item37-voitem37) > 10 | (voitem37-item37) > 1) & 
    (oitem37-item37 > 1.5*(voitem37-oitem37) & oitem37-item37 > 0 |
     oitem37-item37 < 1.5*(voitem37-oitem37) & oitem37-item37 < 0)
	& (item37 > 50 | oitem37 > 3)~1,
	TRUE ~ 0),

	#Glass houses check 
perr10 = case_when((glass > 0.1*oglass | glass < 0.1*oglass) &
    abs(glass-oglass) > 0.02 & (oglass > 0 | glass > 50) &
    (glass > 200*voglass | glass < 0.5*voglass) &
    abs(glass-voglass) > 0.02 &
    (oglass-glass > 1.5*(voglass-oglass) & oglass-glass > 0 |oglass-glass < 1.5*(voglass-oglass) & oglass-glass < 0) ~1,
    TRUE~ 0),
	
	#Land check
#NA check here: SAS includes 3 entries where item12 is missing, whilst R does not....
perr13 = case_when((item12< 1.1*oitem12  & oitem24 == 0 & voitem24  == 0 & item24 > 10
   & ware == 0 & oware > 5 & voware > 5) ~1,
   TRUE~0),

#account for Inf when dividing by 0 i.e if oitem94 = 0 then change to 1
item94_ratio = case_when(oitem94 == 0 ~ 0,
                         TRUE ~ item94/oitem94),

#item94_ratio = item94/oitem94,

#is it oags_madeup? SAS code says ags_madeup but this doesn't exist. commented out to match SAS output...
perr14 = case_when((item94>=50| oitem94 >= 50) && ((item94_ratio >1.8) |(item94_ratio<= 0.5 &item94_ratio>0)) ~1,
                  # oags_madeup == 0 & item94<1 & oitem94>0 ~1,
                   #oags_madeup == 0 & item94>0 &oitem94<1 ~1,
                            TRUE ~0)
)
  


    
  


###will have to change as one form only  for JAC 2023!
all_punch_errors <- all_punch_errors %>% 
  mutate(perr15 = case_when(survtype == "Non-SAF" & (item50>= 2*oitem50 |item50<0.5*oitem50) &
                              (item50>1000 | oitem50 > 1000) ~1,
                            TRUE ~0),
         perr16 = case_when(survtype == "Non-SAF" & (item47>= 2*oitem47 |item47<0.5*oitem47) &
                              (item47>1000 | oitem47 > 1000) ~1,
                            TRUE ~0)
           
         )

filter(all_punch_errors, perr3 > 0)    
  
#perr17 doesn't appear to be computed in SAS code...therefore not mutated here.          
errors <- all_punch_errors %>% filter((sum(perr2, perr3, perr7, perr10, perr13, perr14)>0 & survtype == "Non-SAF")| (sum(perr2, perr3, perr7, perr10, perr14) > 0 & survtype == "SAF"))

