# This code checks for inconsistences in the SAF data and assigns flags to them for validation.
# See June desk notes p24 for validation flags used. 
# This script recreates code in B7 of the SAS June project (\\s0177a\datashare\seerad\ags\census\branch1\NewStructure\Surveys\June\Main\JUNE CENSUS PROJECT - 2021 Provisional Scott)
# This is currently using data from September 2021
# Created by Lucy Nevard 03.02.23 
# Modified by Lucy Nevard 27.02.23




# Before import -----------------------------------------------------------

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

# Load functions

source("Functions/Functions.R")

# Values for checking against

under_reportlimit <- 500
over_report_limit <- 5
under_report_percent <- 0.5
over_report_percent <- 1.1




# Import SAF data -------------------------------------------------------------


allsaf_perm<-loadRData(paste0(Code_directory, "/allsaf_perm_B6.rda"))
allsaf_seas<-loadRData(paste0(Code_directory, "/allsaf_seas_B6.rda"))

# Change variable types if necessary

allsaf_perm$area<-as.numeric(allsaf_perm$area)



# Permanent dataset - checks and error flags -----------------------------------



# Check frequency of fids - multiple holdings using the same fid. I've put a pin in this chunk while we check if the SAS code is actually correct - currently it seems weird and has no effect anyway. 
# Assign flags 1 and 7 as zero for now
fidcheck<-allsaf_perm

fidfreqs<-table(allsaf_perm$fid)




allsaf_perm$flag1<-0
allsaf_perm$flag7<-0

# Note for future: where does the LMC claimtype come from? Check this.

# Dataframes created for different errors etc. This all needs tidying up in the future. 
# Check for differences between total land use and field area (over the overreportlimit or overpercent)

checkarea<-allsaf_perm %>%
  filter(claimtype!="LMC") %>% 
  select(fid, area, field_area, eligible_area, land_use_area, flag1, flag7)
  



checkareasummary<-group_by(checkarea, fid) %>% 
  summarize(sum_area = sum(area),
            sum_field = sum(field_area),
            sum_eligible = sum(eligible_area),
            max_field= max(field_area),
            var_field = var(field_area),
            flag1=max(flag1),
            flag7 = max(flag7))



inconsistentfieldareas<-checkareasummary%>%
  filter(var_field>0)




checkareamismatches<-checkareasummary %>% 
 mutate(diff = round(max_field-sum_area, 3),
        ratio = round(sum_area/max_field, 3))


checkareamismatches<-checkareamismatches %>%
  filter(diff>500|diff<(-5)|ratio>1.1|ratio<0.5&max_field>0&sum_area>0&sum(flag1)>=0)


checkareamismatches<-checkareamismatches %>% 
  select(fid, sum_area, max_field, diff, ratio)

checkareamismatches_fids<-merge(checkareamismatches,allsaf_perm, by="fid")



dperror<-merge(checkareasummary, allsaf_perm, by="fid")

dperror<-dperror %>%
  mutate(dp_ratio = signif(area/(field_area-sum_area+area),3))


dperror<-dperror %>%
  filter(dp_ratio==0.01|dp_ratio==0.1|dp_ratio==10|dp_ratio==100)


areaoverreported<-checkareamismatches_fids%>%
  filter(ratio>1.1 | diff<(-5))


# Remove duplicates in area_over_reported - keeps SFPS over OTHER (Other )

areaoverreported$claimtype<-as.factor(areaoverreported$claimtype)

remove_duplicates<-areaoverreported %>%
  group_by(fid,area,code) %>% 
  filter(!(claimtype=="OTHER" & n() > 1)) 

duplicates<-areaoverreported %>% 
  group_by(fid,area,code) %>% 
  filter((claimtype=="OTHER" & n() > 1))


areastilloverreported<-group_by(remove_duplicates, fid) %>% 
  summarize(
            max_field=max(field_area),
            sum_area=sum(area))


areastilloverreported<-merge(areastilloverreported, allsaf_perm, by="fid")

areastilloverreported<-areastilloverreported%>%
  mutate(
    diff = max_field -sum_area,
    ratio = signif(sum_area / max_field,3))


areastilloverreported<-areastilloverreported%>%
  filter(ratio>over_report_percent|diff<(-over_report_limit))



overreportedexclerror<-areastilloverreported%>%
  filter(code=="EXCL")


areastilloverreported2<-areastilloverreported %>% 
  filter(code!="EXCL")
 
areastilloverreported2<-group_by(areastilloverreported2, fid) %>% 
 summarize(
    max_field=max(field_area),
    sum_area=sum(area))



areastilloverreported2<-merge(areastilloverreported2, allsaf_perm, by="fid")


areastilloverreported2<-areastilloverreported2%>%
  mutate(
    diff = max_field -sum_area,
    ratio = signif(sum_area / max_field,3))


areastilloverreported2<-areastilloverreported2%>%
  filter(ratio>over_report_percent|diff<(-over_report_limit))


# In SAS the following df contains 3 records - investigate why they have an LLO flag - could be because the SAS code B seciton uses August 2021 data. Check this. 

overreportedlloerror<-areastilloverreported %>% 
  filter(LLO=="Y")


areastilloverreported3<-areastilloverreported2 %>% 
  filter(LLO!="Y")

areastilloverreported3<-group_by(areastilloverreported3, fid) %>% 
  summarize(
    max_field=max(field_area),
    sum_area=sum(area))



areastilloverreported3<-merge(areastilloverreported3, allsaf_perm, by="fid")

areastilloverreported3<-areastilloverreported3%>%
  mutate(
    diff = max_field -sum_area,
    ratio = signif(sum_area / max_field,3))

# the following df has 4 more entries in SAS - 3 because of LLO, what about 4th?

areastilloverreported3<-areastilloverreported3%>%
  filter(ratio>over_report_percent)

# get unique llo errors

overreportedllofids<-overreportedlloerror %>%
  group_by(fid) %>% 
  filter(!(n() > 1))


overreportedllofids<-overreportedllofids %>% 
  select(fid, mlc) %>%  
  rename(llomlc=mlc)


  
overreportedothererror<-merge(overreportedllofids,areastilloverreported3, by="fid", all=TRUE )


overreportedothererror<-overreportedothererror %>% 
  filter(claimtype=="OTHER")

# Note: check all dfs are same type

allsaf_perm<-allsaf_perm %>% 
  mutate(crops=code,
         flag6= 
           ifelse(parish<=0 | holding<=0, 1, 0))


duplicates$flag2<-1
duplicates<-duplicates %>% 
  select(brn, fid, line, claimtype, code, area, flag2)

#overreportedlloerror$flag3<-1 this doesn't work on an empty dataframe.

overreportedlloerror<-overreportedlloerror %>% 
  select(brn, fid, line, claimtype, code, area) #include flag3 when df isn't empty


dperror$flag5<-1

dperror<-dperror %>% 
  select(brn, fid, line, claimtype, code, area, dp_ratio, sum_area, flag5)

overreportedothererror$flag8<-1
overreportedothererror<-overreportedothererror %>% 
  select(brn, fid, line, claimtype, code, area, flag8)

overreportedexclerror$flag9<-1

overreportedexclerror<-overreportedexclerror %>% 
  select(brn, fid, line, claimtype, code, area,flag9)


# Create list of all dfs, including errors with their flags. 

df_list<-list(allsaf_perm, duplicates, overreportedlloerror, dperror, overreportedothererror, overreportedexclerror)

# Merge all data frames in list


finalsaf_perm<-df_list %>% reduce(full_join, by=c("brn", "fid", "line", "claimtype", "code", "area"))






# Seasonal dataset - checks and error flags----------------------------------------------------------------

allsaf_seas<-allsaf_seas %>% 
  mutate( 
    flag4= 
      ifelse(mlc==slc, 1, 0))


checkarea_seas<-allsaf_seas %>% 
  filter(claimtype!="LMC")
  
  checkarea_seas<-group_by(checkarea_seas, fid) %>% 
  summarize(sum_area = sum(area),
            sum_field = sum(field_area),
            sum_eligible = sum(eligible_area),
            max_field= max(field_area),
            var_field = var(field_area))
  
  inconsistentfieldareas_seas<-checkarea_seas %>% 
    filter(var_field>0)

  
  checkareamismatches_seas<-checkarea_seas %>% 
    filter(max_field>0&sum_area>0) %>% 
    mutate(diff = round(max_field-sum_area, 3),
           ratio = round(sum_area/max_field, 3))
  
  
  checkareamismatches_seas<-checkareamismatches_seas %>% 
    filter(diff>under_reportlimit|diff<(-over_report_limit)|ratio>over_report_percent|ratio<under_report_percent)

  
  checkareamismatches_seas<-checkareamismatches_seas %>% 
    select(fid, sum_area, max_field, diff, ratio)

  
  checkareamismatches_fids_seas<-merge(checkareamismatches_seas,allsaf_seas, by="fid")

  
  checkarea_seas<-checkarea_seas %>% 
    select(fid,sum_area, max_field)
  
  dperror_seas<-merge(checkarea_seas, allsaf_seas, by="fid")
  
  dperror_seas<-dperror_seas %>%
    mutate(dp_ratio = signif(area/(field_area-sum_area+area),3))

  # Note: the SAS code outputs only 4 observations here (August 2021 dataset)
  
  dperror_seas<-dperror_seas %>%
    filter(dp_ratio==0.01|dp_ratio==0.1|dp_ratio==10|dp_ratio==100)
  
  
  areaoverreported_seas<-checkareamismatches_fids_seas %>% 
    filter(ratio>over_report_percent|diff<(-over_report_limit))
  
# order by Business Name
  
 areaoverreported_seas<- areaoverreported_seas[order(areaoverreported_seas$`Business Name`), ] 
  
remove_duplicates_seas<-areaoverreported_seas[!duplicated(areaoverreported_seas[c("fid","area","code")]),]

duplicates_seas<-areaoverreported_seas[duplicated(areaoverreported_seas[c("fid","area","code")]),]



areastilloverreported_seas<-group_by(remove_duplicates_seas, fid) %>% 
  summarize(max_field= max(field_area),
         sum_area=sum(area))


areastilloverreported_seas<-areastilloverreported_seas %>% 
  select(fid, max_field, sum_area)


areastilloverreported_seas<-merge(areastilloverreported_seas,allsaf_seas, by="fid")

areastilloverreported_seas<-areastilloverreported_seas %>% 
  mutate(diff = max_field -sum_area,
         ratio = signif(sum_area / max_field,3)
         )


areastilloverreported_seas<-areastilloverreported_seas%>%
  filter(ratio>over_report_percent|diff<(-over_report_limit))

# Flag duplicates in seasonal dataset

allsaf_seas<-allsaf_seas %>% 
  mutate(crops=code,
         flag6= 
           ifelse(parish<=0 | holding<=0, 1, 0))


duplicates_seas<-duplicates_seas %>% 
  select(brn, fid, line, claimtype, area, code) %>% 
  mutate(flag2=1)


dperror_seas<-dperror_seas %>% 
  select(brn, fid, line, claimtype, area, code, dp_ratio,sum_area) %>% 
  mutate(flag5=1)


df_list_seas<-list(allsaf_seas,duplicates_seas,dperror_seas)

finalsaf_seas<-df_list_seas %>% reduce(full_join, by=c("brn", "fid", "line", "claimtype", "code", "area"))


# Save to datashare


save(finalsaf_perm,file=paste0(Code_directory, "/allsaf_perm_B7.rda"))
save(finalsaf_seas,file=paste0(Code_directory, "/allsaf_seas_B7.rda"))

# Save to ADM
# 
# write_dataframe_to_db(server=server, 
#                                             database=database,
#                                             schema=schema,
#                                             table_name="allsaf_perm_B7",
#                                             dataframe=finalsaf_perm,
#                                             append_to_existing = FALSE,
#                                             batch_size=1000,
#                                             versioned_table=FALSE)


# In future, save other dfs made above if needed in the code downstream


