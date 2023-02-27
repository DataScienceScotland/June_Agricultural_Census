# This script changes SAF codes to JAC item numbers. 
# This script recreates code in B9 of the SAS June project (\\s0177a\datashare\seerad\ags\census\branch1\NewStructure\Surveys\June\Main\JUNE CENSUS PROJECT - 2021 Provisional Scott)
# Created by Lucy Nevard 17.02.23 
# Modified by Lucy Nevard 27.02.23

# Clear environment prior 

rm(list=ls())

# Datashare file path for import and export

Code_directory <- ("//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Codeconversion_2023")

# ADM schema for export

server <- "s0196a\\ADM"
database <- "RuralAndEnvironmentalScienceFarmingStatistics"
schema <- "juneagriculturalsurvey2023alpha"

# Load packages

library(tidyverse)
library(stringr)
library(data.table)


# Load functions

source("Functions/Functions.R")



# Import all saf data and code translation table

allsaf<-loadRData(paste0(Code_directory,"/allsaf_B8.rda"))

newcodetrans<-read.csv(paste0(Code_directory,"/NEW_CODE_TRANS21.csv"))




check.llo<-allsaf %>% 
  filter(!(LLO=="N"|LLO=="Y"))


summ_fds<-allsaf %>% 
  filter(!claimtype=="LMC") %>% 
  mutate(LLO_area=
           ifelse(LLO=="Y", area, 0),
         LFASS_area=
           ifelse((lfass_eligible=="Y"&claimtype=="SFPS"),area,0))

allsaf_fids<-summ_fds %>% 
  group_by(parish, holding, fid, code) %>% 
  summarize(
            mlc=unique(mlc),
            slc=unique(slc),
            brn=unique(brn),
            area=sum(area),
            LLO_area=sum(LLO_area),
            LFASS_area=sum(LFASS_area),
            field_area=max(field_area),
            eligible_area=max(eligible_area),
            .groups="keep")

rm(summ_fds)

aggregate1<-allsaf_fids %>% 
  group_by(parish, holding, code) %>% 
  summarize(
            area=sum(area),
            LLO_area=sum(LLO_area),
            LFASS_area=sum(LFASS_area),
            mlc=unique(mlc),
            slc=unique(slc),
            brn=unique(brn),
            .groups="keep")

allsaf_reduced<-allsaf %>% 
  select(parish, holding, mlc, brn, area)

rm(allsaf, allsaf_fids,check.llo)

# Translate codes to June items based on translation table (this will probably be updated every year)

aggregate1$code<-as.factor(aggregate1$code)
newcodetrans21$code<-as.factor(newcodetrans21$code)

cens_coded<-merge(aggregate1,newcodetrans,by="code", all.x = TRUE)

unmatched_codes<-cens_coded [ !aggregate1$code %in% cens_coded$code ,]

rm(newcodetrans, aggregate1)

allsaf<-as_tibble(allsaf)

# Produces item185 for item 41 (item41 is Unspecified Crops Total Area). Item185 will specify the crops. 


extra_ncode<-cens_coded %>% 
  select(parish, holding, cens_code, code)

extra_ncode$item185<-""

extra_ncode<-extra_ncode %>% 
  group_by(parish, holding) %>% 
  mutate(item185=
           ifelse(row_number()==1,"a",""))

# These will probably change 
extra_ncode<-extra_ncode %>% 
  group_by(parish, holding) %>%
     mutate(ncode=
           ifelse(cens_code=="item41",
                  ifelse(code=="ALMS","Almonds",
                    ifelse(code=="BW", "Buckwheat",
                         ifelse(code=="CANS","Canary seed",
                                ifelse(code=="EX-SS","Ex-structural set-aside",
                                       ifelse(code=="FFS", "Fibre flax",
                                              ifelse(code=="GCM","Green cover mixtures",
                                                     ifelse(code=="HS","Hemp",
                                                            ifelse(code=="MIL","Millet",
                                                                   ifelse(code=="MSC", "Miscanthus",
                                                                          ifelse(code=="PEM","Positive environmental management",
                                                                                 ifelse(code=="RCG","Reed canary grass",
                                                                                        ifelse(code=="RYE","Rye",
                                                                                               ifelse(code=="SOR","Sorghum",
                                                                                                      ifelse(code=="SRC","Short rotation coppice",
                                                                                                             ifelse(code=="TURF","Turf production",
                                                                                                                    ifelse(code=="WBS","Wild bird seed", code)))))))))))))))),""))

extra_ncode<-extra_ncode %>% 
  group_by(parish, holding) %>%
  mutate(item185=
           ifelse(cens_code=="item41",
                  ifelse(item185!="a",paste(item185,ncode), ncode ),""))

extra_ncode$item185 [is.na(extra_ncode$item185)] <- ""

# Group by parish and holding. item185 is multiple strings concatenated - should change this to include semicolon (if collapse=";" it ends up with lots of unwanted semicolons!)

extra_item185<-extra_ncode %>% 
 group_by(parish, holding) %>% 
  summarise(item185=paste(item185,collapse=""))


extra_item185<-extra_item185 %>% 
  mutate(
    parish=as.numeric(parish),
    holding=as.numeric(holding))

rm(extra_ncode)

# Group by SLC

census_format<-cens_coded %>% 
  group_by(parish, holding, cens_code) %>% 
  summarise(area = sum(area),
            LLO_area=sum(LLO_area),
            LFASS_area=sum(LFASS_area),
          mlc = unique(mlc),
          brn = unique(brn))

# Extra fields to fold in later

extra_fields<-census_format %>% 
  group_by(parish, holding) %>% 
  summarise(LLO_area=sum(LLO_area),
            LFASS_area=sum(LFASS_area),
            mlc = unique(mlc),
            brn = unique(brn))


brns<-allsaf_reduced 

rm(allsaf_reduced)

brns$parish<-str_pad(brns$parish, width=3, pad="0")
brns$holding<-str_pad(brns$holding, width=4, pad="0")



brns<-brns %>% 
  select(parish, holding, brn, mlc, area) %>% 
  mutate(location_type=
           paste0(parish,"/",
                  holding)) %>% 
  mutate(location_type=
         ifelse(location_type==mlc,"Main","Subcode"))




brns<-brns %>% 
  group_by(parish,holding,brn) %>% 
  summarise(area=sum(area),
            mlc = unique(mlc),
            location_type=unique(location_type))



brns<-brns %>% 
  group_by(parish,holding)
summarize_all(distinct)


cens_wide<-census_format %>% 
  select(parish, holding, cens_code,area)  
  
cens_wide<-cens_wide %>%
  group_by(parish,holding) %>% 
  pivot_wider(names_from=cens_code, values_from=area)



# Check where these items are in the JAC
  
order<-c("parish",  "holding",  "item2321", "item2322", "item2828", "item9999", "item2469", "item2470", "item3156", "item47", "item20", 
           "item16", "item14", "item19", "item41", "item70","item48","item18","item2320","item66", "item17","item24", 
           "item2827", "item34", "item29", "item32","item49","item31","item30", "item28", "item52", "item1710", 
           "item83",  "item53", "item63", "item75", "item27", "item56", "item2858", "item82", "item80", "item2879", "item21", "item23", 
           "item15", "item36", "item2059",  "item2323", "item64", "item71",  "item72", "item2859", "item2324", "item1709", "item2860",
           "item65", "item60",  "item59", "item2832", "item61", "item2034", "item2861", "item55", "item81", "item2707", "item22","item2863",  "item2864",  "item2865")
  

  
  
  
orderdf<- order[order %in% colnames(cens_wide)]

cens_wide<-setDT(cens_wide)
cens_wide<-setcolorder(cens_wide, as.character(orderdf))

# add column not in dataset and reorder to include

addtodf<-order[!order %in% colnames(cens_wide)]

cens_wide[ , addtodf] <- NA

cens_wide<-setcolorder(cens_wide, as.character(order))


cens_wide[cens_wide=='NULL'] <- 0

cens_wide[is.na(cens_wide)] <- 0

cens_wide <- cens_wide %>%
  rename_at('NA', ~'unmatched')


cens_wide<-data.frame(lapply(cens_wide, function(x)unlist((as.character(x)))))


cens_wide<-data.frame(lapply(cens_wide, function(x)(as.numeric(as.character(x)))))


cens_wide<-cens_wide %>% 
  filter(!parish<1 | !holding<1)


# Create final saf dataset



extra_fields<-extra_fields %>% 
  select(parish, holding, LFASS_area, LLO_area)


extra_fields$parish<-as.numeric(extra_fields$parish)
extra_fields$holding<-as.numeric(extra_fields$holding)

brns<-brns %>% 
  select(parish, holding, brn, mlc)

# remove leading zeroes

brns$parish<-sub("^0+", "", brns$parish)    
brns$holding<-sub("^0+", "", brns$holding)    
brns$parish<-as.numeric(brns$parish)
brns$holding<-as.numeric(brns$holding)



cens_wide2<-left_join(cens_wide,extra_fields, by=c("parish", "holding"))

cens_wide3<-left_join(cens_wide2, extra_item185, by=c("parish", "holding"))


cens_wide_final<-left_join(cens_wide3, brns,  by=c("parish", "holding"))  # check that this hasn't added rows 



                             
cens_wide_final<-cens_wide_final %>% 
  mutate(
    item1=LLO_area,
    item2=item2879+item2827+item2828,
    item68=item52+ item53+ item55+ item56+ item2323+ item59+ item60+ item61+item63+ item64+ item65+ item66,
    item76=item70+ item71+ item72+ item2832+ item75,
    item84=item2324+ item1709+ item80+ item81+ item1710+ item82+ item83,
    item35=item68,
    item37=item76,
    item38=item41+item84+item2858+ item2863+ 
               item2859+ item2864+ 
               item2860+ item2865+ 
               item2861,
    item40=item14+ item15+ item16+ item18+ item17+ item20+ item3156+ item22+ 
               item19+ item23+ item21+ item24+ item2320+ item27+ item28+ 
               item2034+ item29+ item30+ item31+ item2059+ item32+ item34+ item36+ item2469+ item2470+ item35+ item37+ item38,
    item46=item2321+ item2322+ item40,
    saf_land=
      ifelse((item47+ item49+ item46+ item9999)>0, 1, "")
    
  )

                  
# Check what order the variables should be in - does it matter. 



cens_wide_final[cens_wide_final=='NULL'] <- 0
cens_wide_final[is.na(cens_wide_final)] <-0

# Remove any stray duplicates. 

cens_wide_dups<-cens_wide_final[!duplicated(cens_wide_final[,1:2]),]



# Note: decide what checks to add here. e.g. SAS code checks if item50 exists (total land) and is > 0 in any cases. Item50 is not in the dataset.

# Save to datashare 

save(cens_wide_final,file=paste0(Code_directory,"/allsaf_final.rda"))

