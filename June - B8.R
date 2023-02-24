# This script recreates code in B8 of the SAS June project (\\s0177a\datashare\seerad\ags\census\branch1\NewStructure\Surveys\June\Main\JUNE CENSUS PROJECT - 2021 Provisional Scott)
# Created by Lucy Nevard 10.02.23 
# Modified by Lucy Nevard 17.02.23

# Clear environment prior 

rm(list=ls())

# Load packages

library(tidyverse)
library(dplyr)
library(readxl)
library(sqldf)
# Functions

loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}



allsaf_perm21<-loadRData("//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Codeconversion_2023/allsaf_perm_B7end.rda")
allsaf_seas21<-loadRData("//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Codeconversion_2023/allsaf_seas_B7end.rda")


allsaf_seas21_fid<-allsaf_seas21 %>% 
  rename(area21=area)


# Last year's data must be read in from a csv - R can't handle the xlsx
allsaf_20<-read.csv("//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Codeconversion_2023/ALLSAF20.csv")
                    
                  
allsaf_seas20_fid<- allsaf_20 %>% 
  filter(substr(code,1,4)=="LLI-" & claimtype =="SFPS") %>% 
  rename(area20=area)


allsaf_seas20_fid<- allsaf_seas20_fid %>% 
  distinct(parish,holding,fid, .keep_all=TRUE)

allsaf_seas20_fid<-allsaf_seas20_fid %>% 
  filter(!(is.na(parish)|is.na(holding)))




saf_seas20_cph<-allsaf_20 %>% 
  select(parish,holding)

saf_seas20_cph<-saf_seas20_cph %>% 
  filter(!(is.na(parish)|is.na(holding)))

# remove duplicates

saf_seas20_cph<-saf_seas20_cph %>% 
  distinct(parish,holding, .keep_all=TRUE)


saf_seas20_cph<-saf_seas20_cph %>% 
  mutate(parish=as.numeric(parish),
         holding=as.numeric(holding))

allsaf_seas21_fid<-allsaf_seas21_fid %>% 
  select(-c("other_area","other_code"))

allsaf_seas21_fid<-allsaf_seas21_fid %>% 
  mutate(parish=as.numeric(parish),
         holding=as.numeric(holding))

allsaf_seas21_fid<-allsaf_seas21_fid %>%
  filter(!(is.na(parish)|is.na(holding)|is.na(fid)))


# The SAS code for this keeps NAs (parish and holding) in as if they are matching - I have already removed parish/holding NAS from seas21_fid

seas21_matched<-merge(allsaf_seas21_fid,saf_seas20_cph, by=c("parish","holding"))


only21_fid <- setdiff(allsaf_seas21_fid,seas21_matched)

allsaf_seas20_fid2<-allsaf_seas20_fid %>% 
  select(area20,parish,holding,fid)

bothyears<-merge(seas21_matched,allsaf_seas20_fid2,by=c("parish","holding","fid"),all.x=TRUE)



bothyears<-bothyears %>% 
  mutate(code=
           ifelse(area21>0&area20>0, "LLI-SL","LLI-DL"))


bothyears<-bothyears %>% 
  select(-area20)

only21_fid<-only21_fid %>% 
  mutate(code="LLI-NL")


split<-rbind(bothyears,only21_fid)


pfds21_seas<-split %>% 
  rename(area=area21) %>% 
  mutate(LLO=0,
         landtype="SEAS",
         claimtype="SFPS")

# Note: remove flagged entries here if requires  (SAF validations in C) 





pfds_final21<-rbind(allsaf_perm21,pfds21_seas,fill=TRUE)


pfds_corrections1<-pfds_final21 %>% 
  filter(flag6==1 & !is.na(mlc) & landtype=="PERM") %>% 
  mutate(parish = str_remove(substr(mlc,1,3), "^0+"),
         holding = str_remove(substr(mlc,5,8),"^0+"),
         flag6=0)


pfds_corrections2<-pfds_final21 %>% 
  filter((flag1>0 | flag2>0) & !is.na(fid))



pfds_corrections3<-pfds_final21 %>% 
  filter(flag5>0 & !is.na(fid)) %>% 


# The below df doesn't completely match up with the one produced in SAS - check flags in B7

pfdscorrections<-rbind(pfds_corrections1,pfds_corrections2,pfds_corrections3)

# Following chunk only works when flag3 is present - check B7 code. 

# pfds_final21<-pfds_final21 %>% 
#   filter(!flag3>0)


pfds_final21<-setdiff(pfds_final21,pfds_corrections2)


pfds_final21<-pfds_final21 %>% 
  mutate(area=
           ifelse(flag5>0 & !is.na(flag5), (area/dp_ratio),area))


pfds_final21<-pfds_final21 %>% 
  mutate(flag5=0,
         flag6=0)



allsaf_perm21<-pfds_final21 %>% 
  filter(landtype=="PERM")


allsaf_seas21<-pfds_final21 %>% 
  filter(landtype=="SEAS")


allsaf_21<-pfds_final21 %>% 
  select(-c(`Business Name`,land_use,land_use_area,bps_claimed_area,APPLICATION_STATUS,IS_PERM_FLAG,sfp_area,sfp_code,dp_ratio,sum_area))



save(allsaf_21,file="//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Codeconversion_2023/allsaf_21.rda")
