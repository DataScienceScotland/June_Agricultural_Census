# This code splits the SAF data seasonal fids into different LLIs, combines the permanent and seasonal data into one allsaf dataset
# This script recreates code in B8 of the SAS June project (\\s0177a\datashare\seerad\ags\census\branch1\NewStructure\Surveys\June\Main\JUNE CENSUS PROJECT - 2021 Provisional Scott)
# This currently uses data from September 2021
# Created by Lucy Nevard 10.02.23 
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
library(dplyr)
library(sqldf)

# Functions

source("Functions/Functions.R")



allsaf_permcurr<-loadRData(paste0(Code_directory,"/allsaf_perm_B7.rda"))
allsaf_seascurr<-loadRData(paste0(Code_directory,"/allsaf_seas_B7.rda"))

# Last year's data must be read in from a csv - R struggles with the xlsx

allsaf_prev<-read.csv(paste0(Code_directory,"/ALLSAF20.csv"))

# rename area in current seasonal data

allsaf_seascurr_fid<-allsaf_seas %>% 
  rename(areacurr=area)


# Split seasonal data into different LLIs

                    
                  
allsaf_seasprev_fid<- allsaf_prev %>% 
  filter(substr(code,1,4)=="LLI-" & claimtype =="SFPS") %>% 
  rename(areaprev=area)


allsaf_seasprev_fid<- allsaf_seasprev_fid %>% 
  distinct(parish,holding,fid, .keep_all=TRUE)

allsaf_seasprev_fid<-allsaf_seasprev_fid %>% 
  filter(!(is.na(parish)|is.na(holding)))




saf_seasprev_cph<-allsaf_prev %>% 
  select(parish,holding)

saf_seasprev_cph<-saf_seasprev_cph %>% 
  filter(!(is.na(parish)|is.na(holding)))

# remove duplicates

saf_seasprev_cph<-saf_seasprev_cph %>% 
  distinct(parish,holding, .keep_all=TRUE)


saf_seasprev_cph<-saf_seasprev_cph %>% 
  mutate(parish=as.numeric(parish),
         holding=as.numeric(holding))

allsaf_seascurr_fid<-allsaf_seascurr_fid %>% 
  select(-c("other_area","other_code"))

allsaf_seascurr_fid<-allsaf_seascurr_fid %>% 
  mutate(parish=as.numeric(parish),
         holding=as.numeric(holding))

allsaf_seascurr_fid<-allsaf_seascurr_fid %>%
  filter(!(is.na(parish)|is.na(holding)|is.na(fid)))

# Different LLI types are assigned here.

# 	- LLI-SL = Land seasonally let in at same location as last year				
# - LLI-DL = Land seasonally let in at a different location to last year	
# - LLI-NL = Land seasonally let in but no SAF claimed last year	

# The SAS code for this keeps NAs (parish and holding) in as if they are matching - I have already removed parish/holding NAS from seas21_fid

seascurr_matched<-merge(allsaf_seascurr_fid,saf_seasprev_cph, by=c("parish","holding"))


onlycurr_fid <- setdiff(allsaf_seascurr_fid,seascurr_matched)

allsaf_seasprev_fid2<-allsaf_seasprev_fid %>% 
  select(areaprev,parish,holding,fid)

bothyears<-merge(seascurr_matched,allsaf_seasprev_fid2,by=c("parish","holding","fid"),all.x=TRUE)



bothyears<-bothyears %>% 
  mutate(code=
           ifelse(areacurr>0&areaprev>0, "LLI-SL","LLI-DL"))


bothyears<-bothyears %>% 
  select(-areaprev)

onlycurr_fid<-onlycurr_fid %>% 
  mutate(code="LLI-NL")


split<-rbind(bothyears,onlycurr_fid)


pfdscurr_seas<-split %>% 
  rename(area=areacurr) %>% 
  mutate(LLO=0,
         landtype="SEAS",
         claimtype="SFPS")

# Note: remove flagged entries if required  (SAF validations also in C) 


pfds_finalcurr<-rbind(allsaf_permcurr,pfdscurr_seas,fill=TRUE)


pfds_corrections1<-pfds_finalcurr %>% 
  filter(flag6==1 & !is.na(mlc) & landtype=="PERM") %>% 
  mutate(parish = str_remove(substr(mlc,1,3), "^0+"),
         holding = str_remove(substr(mlc,5,8),"^0+"),
         flag6=0)


pfds_corrections2<-pfds_finalcurr %>% 
  filter((flag1>0 | flag2>0) & !is.na(fid))



pfds_corrections3<-pfds_finalcurr %>% 
  filter(flag5>0 & !is.na(fid)) %>% 


# The below df doesn't completely match up with the one produced in SAS - possibly because SAS is on August 2021 data. 

pfdscorrections<-rbind(pfds_corrections1,pfds_corrections2,pfds_corrections3)

# Following chunk only works when flag3 is present - see B7 cscript. 

# pfds_finalcurr<-pfds_finalcurr %>% 
#   filter(!flag3>0)


pfds_finalcurr<-setdiff(pfds_finalcurr,pfds_corrections2)


pfds_finalcurr<-pfds_finalcurr %>% 
  mutate(area=
           ifelse(flag5>0 & !is.na(flag5), (area/dp_ratio),area))


pfds_finalcurr<-pfds_finalcurr %>% 
  mutate(flag5=0,
         flag6=0)



allsaf_permcurr<-pfds_finalcurr %>% 
  filter(landtype=="PERM")


allsaf_seascurr<-pfds_finalcurr %>% 
  filter(landtype=="SEAS")


allsaf_curr<-pfds_finalcurr %>% 
  select(-c(`Business Name`,land_use,land_use_area,bps_claimed_area,APPLICATION_STATUS,IS_PERM_FLAG,sfp_area,sfp_code,dp_ratio,sum_area))

# Save combined allsaf dataset to datashare 

save(allsaf_curr,file=paste0(Code_directory,"/allsaf_B8.rda"))
