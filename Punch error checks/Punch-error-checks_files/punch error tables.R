#Tables for punch error checks------------------------------------------
#can use June items from Item categories file or, if using with Rmd items appear in "punch item vars" section 

# Before import -----------------------------------------------------------


# Clear environment prior
rm(list=ls())

###check if this has been updated for this year!!
#commented out because Rmd doesn't work with another "source" ref
#source("Punch error checks/Punch-error-checks_files/Item categories.R")



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

# Load functions and item lists--------------------------------------------------------

punch_June_items <- c("item7", "item11", "item12", "item40", "item157", "item170", "item2321", "item145", "item139", "item140", "item141", "item143", "item144", "item47", "item49", "item2322", "item37", "item2469", "item2470", "item85", "item86", "item24", "item2320", "item160", "item162", "item163", "item94", "item50", "item2511")

#prev years items seem to be the same as "this year's"

punch_June_items_yr1 <- punch_June_items

punch_June_items_yr2 <- punch_June_items

perr1_vars<-c("perr1", "item12", "oitem12", "voitem12", "grass", "ograss", "vograss", "othgrass", "oothgrass", "voothgrass", "item47", "oitem47", "voitem47", "tillage", "otillage", "votillage") 
perr2_vars <- c("perr2", "item157", "oitem157", "voitem157")
perr3_vars <- c("perr3", "item170", "oitem170", "voitem170")
perr4_vars <- c("perr4", "cts312", "octs312", "vocts312")
perr5_vars <- c("perr5", "dairy", "odairy", "vodairy")
perr6_vars <- c("perr6","beef", "obeef", "vobeef")
perr7_vars <- c("perr7", "item145", "oitem145", "voitem145")
perr9_vars <- c("perr9",  "item37", "oitem37", "voitem37")
perr10_vars <- c("perr10", "glass", "oglass", "voglass")
perr11_vars <- c("perr11", "male", "omale", "vomale")
perr12_vars <- c("perr12", "female", "ofemale", "vofemale")
perr13_vars <- c("perr13", "ware", "oware", "voware")
perr14_vars <- c("perr14", "item94", "oitem94", "voitem94")
perr15_vars <- c("perr15", "item50", "oitem50", "voitem50")
perr16_vars <- c("perr16", "item47", "oitem47", "voitem47")
perr17_vars <- c("perr17", "item2511", "oitem2511", "voitem2511")




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
         oglass=sum(sum(oitem87, oitem2556, oitem2557, oitem2036, oitem2037, oitem1711, oitem1943, oitem2836)),
         voglass=sum(sum(voitem87, voitem2556, voitem2557, voitem2036, voitem2037, voitem1711, voitem1943)),
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
    #Poulty check - if total poultry is greater than 8 times or less than an eighth last year's poultry and the land area has not as much as doubled----------------------------
 
    perr3 = case_when (((item170 > 8.*oitem170 & item12 < 2*oitem12) | (item170 < 0.125*oitem170 & item12 > 0.5*oitem12)) &
    ((item170-oitem170) > 150000 | (oitem170-item170) > 100000) &
    (item170 > 8.*voitem170 | item170 < 0.125*voitem170) &
    ((item170-voitem170) > 150000 | (voitem170-item170) > 100000) &
    (oitem170-item170 > 1.5*(voitem170-oitem170) & oitem170-item170 > 0 |
     oitem170-item170 < 1.5*(voitem170-oitem170) & oitem170-item170 < 0) ~1,
    TRUE~0),
	 
	#Sheep check ------------------------------------------------------------------------------------------------------------------------------------------
perr7= case_when(((item145 > 10.*oitem145 & item12 < 2*oitem12) 	| (item145 < 0.1*oitem145 & item12 > 0.5*oitem12)) &
    abs(item145-oitem145) > 5000 & 
    (item145 > 10.*voitem145 | item145 < 0.1*voitem145) &
    abs(item145-voitem145) > 5000 &
    (oitem145-item145 > 1.5*(voitem145-oitem145) & oitem145-item145 > 0 |
     oitem145-item145 < 1.5*(voitem145-oitem145) & oitem145-item145 < 0)~ 1, 
	TRUE~0),
 

	 #Soft fruit check--------------------------------------------------------------------------------------------------------
perr9 =  case_when((item37 > 3.*oitem37 | item37 < 0.3*oitem37) &
    ((item37-oitem37) > 10 | (oitem37-item37) > 1) & 
    (item37 > 3.*voitem37 | item37 < 0.3*voitem37) &
    ((item37-voitem37) > 10 | (voitem37-item37) > 1) & 
    (oitem37-item37 > 1.5*(voitem37-oitem37) & oitem37-item37 > 0 |
     oitem37-item37 < 1.5*(voitem37-oitem37) & oitem37-item37 < 0)
	& (item37 > 50 | oitem37 > 3)~1,
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
perr13 = case_when(((item12< 1.1*oitem12 ) && oitem24 == 0 & voitem24  == 0 & item24 > 10
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
  


###will have to change as one form only  for JAC 2023!--------------------------------------------------------------------------------------------------------------
all_punch_errors <- all_punch_errors %>% 
  mutate(perr15 = case_when(survtype == "Non-SAF" & (item50>= 2*oitem50 |item50<0.5*oitem50) &
                              (item50>1000 | oitem50 > 1000) ~1,
                            TRUE ~0),
         perr16 = case_when(survtype == "Non-SAF" & (item47>= 2*oitem47 |item47<0.5*oitem47) &
                              (item47>1000 | oitem47 > 1000) ~1,
                            TRUE ~0)
           
         )

  
  
##----perr17 doesn't appear to be computed in SAS code...therefore not mutated here.          
errors <- all_punch_errors %>% filter((sum(perr2, perr3, perr7, perr10, perr13, perr14)>0 & survtype == "Non-SAF")| (sum(perr2, perr3, perr7, perr10, perr14) > 0 & survtype == "SAF"))







##move this function---------------------------------------------------------------------------------


#fix function
# add_err_col <- function(x)
# { 
# x %>%  mutate(error=ifelse(perr2== 1, "Error", "No Error"),
#               difference = ifelse(item157-oitem157 < 0, -0.5*(item157-oitem157), item157-oitem157)
# )
# 
# }
#

##----perr 1----------------------------------------------------------------------------------------------
##@knitr perr1

keep_perr_vars <- function(x){
    perr_x <<- errors %>% select(parish, holding, survtype, all_of(x))    
  name_x <<- sub("\\_.*", "", substitute(x))
}

keep_perr_vars(perr1_vars)
assign(name_x, perr_x)
perr1 <-perr1 %>%  mutate(error=ifelse(perr1== 1, "Error", "No Error"),
                          difference = (tillage-otillage))
                          
                          
                          
##----perr 2----------------------------------------------------------------------------------
keep_perr_vars(perr2_vars)
assign(name_x, perr_x)


perr2 <-perr2 %>%  mutate(error=ifelse(perr2== 1, "Error", "No Error"),
                           difference = ifelse(item157-oitem157 < 0, -0.5*(item157-oitem157), item157-oitem157)
)



##----perr 3----------------------------------------------------------------------------------
keep_perr_vars(perr3_vars)
assign(name_x, perr_x)


perr3 <-perr3 %>%  mutate(error=ifelse(perr3== 1, "Error", "No Error"),
                          difference = ifelse(item170 -oitem170 < 0, -0.5*(item170-oitem170), item170-oitem170)
)




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
                          difference = ifelse(item145 -oitem145 < 0, -0.5*(item145-oitem145), item145-oitem145)
)



##----perr9------------------------------------------------------------------------------------------------
keep_perr_vars(perr9_vars)
assign(name_x, perr_x)


perr9 <-perr9 %>%  mutate(error=ifelse(perr9== 1, "Error", "No Error"),
                          difference = ifelse(item37 -oitem37 < 0, -0.5*(item37-oitem37), item37-oitem37)
)


##----perr10------------------------------------------------------------------------------------------------
keep_perr_vars(perr10_vars)
assign(name_x, perr_x)


perr10 <-perr10 %>%  mutate(error=ifelse(perr10== 1, "Error", "No Error"),
                          difference = ifelse(glass -oglass < 0, -0.5*(glass-oglass), glass-oglass)
)



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



##----perr14------------------------------------------------------------------------------------------------
keep_perr_vars(perr14_vars)
assign(name_x, perr_x)


perr14 <-perr14 %>%  mutate(error=ifelse(perr14== 1, "Error", "No Error"))

##----perr15------------------------------------------------------------------------------------------------
keep_perr_vars(perr15_vars)
assign(name_x, perr_x)


perr15 <-perr15 %>%  mutate(error=ifelse(perr15== 1, "Error", "No Error"))


##----perr16------------------------------------------------------------------------------------------------
keep_perr_vars(perr16_vars)
assign(name_x, perr_x)


perr16 <-perr16 %>%  mutate(error=ifelse(perr16== 1, "Error", "No Error"))


# ##----perr17------------------------------------------------------------------------------------------------
# keep_perr_vars(perr17_vars)
# assign(name_x, perr_x)
# 
# 
# perr17 <-perr17 %>%  mutate(error=ifelse(perr17== 1, "Error", "No Error"))
# 
# 
#

##----perr-summary---------------------------------------------------------------------------------------------
error_count <- all_punch_errors %>% ungroup() %>% 
  select(survtype, starts_with("perr")) %>% group_by(survtype)

  error_count_n <- error_count %>% summarise(count = n())
  
#to fix. For now need to manually type perr variables
error_agg <- aggregate(cbind(perr1, perr2, perr3, perr7, perr9, perr10, perr13, perr14, perr15, perr16) ~ survtype, data = error_count, FUN = sum)

punch_error_summary <- full_join(error_count_n, error_agg, by = "survtype")
  


