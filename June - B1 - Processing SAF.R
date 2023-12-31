# This script processes the SAF data to prepare it for combining with the census data in B2. It also flags potential errors in the SAF dataset. 
# Code corrections may need updating yearly depending on SAF data.
# This script is based on the code in B4 and B6 of the June Project (\\s0177a\datashare\seerad\ags\census\branch1\NewStructure\Surveys\June\Main\JUNE CENSUS PROJECT - 2021 Provisional)
# Data used currently is from end of June 2023. This is the second SAF data drop.
# Created by Lucy Nevard 27.01.23
# Modified by Lucy Nevard 25.09.23


# Before import -----------------------------------------------------------


# Clear environment prior

rm(list = ls())

# Load packages

library(tidyverse)
library(data.table)
library(janitor)
library(RtoSQLServer)
library(haven)


# Load functions

source("Functions/Functions.R")


# Datashare file path for import and export

Code_directory <- ("//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Codeconversion_2023/2023")
sas_agstemp_path <- "//s0177a/sasdata1/ags/census/agstemp/"
sas_agscens_path <- "//s0177a/sasdata1/ags/census/agscens/"

# ADM schema for export

server <- "s0196a\\ADM"
database <- "RuralAndEnvironmentalScienceFarmingStatistics"
schema <- "juneagriculturalsurvey2023alpha"


# Import SAF data -------------------------------------------------------------

# From the ADM server and create list


saf_perm <- read_table_from_db(server=server, 
                               database=database, 
                               schema=schema, 
                               table_name="saf_perm_A_2023")

saf_seas <- read_table_from_db(server=server, 
                               database=database, 
                               schema=schema, 
                               table_name="saf_seas_A_2023")

# Reading in from ADM creates a new variable with id. Remove.

saf_perm<-saf_perm %>% 
  select(-any_of("saf_perm_A_2023ID"))

saf_seas<-saf_seas%>% 
  select(-any_of("saf_seas_A_2023ID"))

list_perm_seas<-list(saf_perm,saf_seas)

# Or from the datashare

# list_perm_seas <- loadRData(paste0(Code_directory, "/saflist_permseas_A.rda"))


# Import new code translation table for SAF (these may need updating every year)


newcodetrans <- read_table_from_db(server=server, 
                                   database=database, 
                                   schema=schema, 
                                   table_name="newcodetrans")


# SAF 2015 dataset for EXCL land


saf2015 <- read_table_from_db(server=server, 
                               database=database, 
                               schema=schema, 
                               table_name="saf2015")



# Initial corrections ----------------------------------------------------------

# This code applies corrections to the SAF datasets (permanent and seasonal). These corrections will include manual corrections which can be added each year, perhaps to the function script.
# Then, for the permanent data, it corrects invalid codes (this will need updating every year) and  splits up lines with both SFP code and Other Code. For the seasonal data, it splits up and removes those with only an Other code.
# Code corrections may need updating yearly depending on SAF data.
# This section is based on the code in B4 and B6 of the June Project


# Correcting area variables and slc when blank. Code corrections for sfp_code and other_code.


# Clean names again to ensure consistency

list_perm_seas<-lapply(list_perm_seas, clean_names)

# For SAF codes - this might change year to year so the change_codes function would need to be updated in the Functions script. But those below might still be used.
# Manual corrections should also mbe included here and change from year to year. None from the 2021 SAS project.

list_perm_seas <- lapply(list_perm_seas, change_codes)



# Create separate dfs for permanent and seasonal. These are processed separately from this point on.

names(list_perm_seas) <- c("perm", "seas")

for (i in seq(list_perm_seas)) {
  assign(paste("saf", names(list_perm_seas)[[i]], sep = "_"), list_perm_seas[[i]])
}


saf_perm$landtype <- "PERM"
saf_seas$landtype <- "SEAS"


# Remove rows of all NAs. This could be moved to Script A.
# There are no rows of all NAs at this point in the 2023 dataset.


saf_perm <- saf_perm[rowSums(is.na(saf_perm)) != ncol(saf_perm), ]

saf_seas <- saf_seas[rowSums(is.na(saf_seas)) != ncol(saf_seas), ]


# Permanent - split up SAF datalines into claimtype other or sfp

# Reformat df with claimtype as "other" or "sfp" (Single Farm Payment) and "line" variable increasing by 0.01 if OTHER.
# LLO flags also assigned within newvarsother and newvarssfp functions

# Other claimtype df

saf_permother <- filter(saf_perm, saf_perm$other_area > 0)

# Create variables in saf_permother

saf_permother <- newvarsother(saf_permother)


# SFP claimtype df

saf_permsfp <- filter(saf_perm, saf_perm$sfp_area > 0)

# Create variables in saf_permsfp

saf_permsfp <- newvarssfp(saf_permsfp)


# Merge Other and SFP


saf_perm<- rbind(saf_permother, saf_permsfp)


# Remove unnecessary variables


saf_perm<- saf_perm %>%
  select(-c(other_area, other_code, prefix))

# Create correct parish and holding from "slc" (Single Location Code)

saf_perm <- parishholdingperm(saf_perm)
                        
# Correct variable types

saf_perm<-perm_variables(saf_perm)
  
# The SAS code here has tables to see what errors are left in the permanent df - not clear what we're looking for at this point and there aren't any fixes in the code, so I've left this out for now.


#  Seasonal - filter out "Other" holdings, keep only SFP
# According to desk notes, the data for "other" is too messy to be reliable. The seasonal data is therefore an underestimate.
# LLO flags also assigned within newvarsseas function

saf_seas <- filter(saf_seas, saf_seas$sfp_area > 0)

# SFP claimtype df - add variables

saf_seas<-newvarsseas(saf_seas)


# Create correct parish and holding from mlc (main location code)

saf_seas<-parishholdingseas(saf_seas)

# Remove unnecessary variables 

saf_seas<-saf_seas %>% 
select(-c(other_area, other_code))

# Correct variable types

saf_seas<-seas_variables(saf_seas)



# Validations and flagging potential errors----------------------------------------------------------

#B7 of the SAS code
# Flag numbers: 
# 1. Field ID (FID) recorded as belonging to multiple holdings (Single Location Code: SLC) on permanent sheets where one reported it as seasonally let out (LLO). A permanent tenancy may incorrectly be recorded as a seasonal let out]'/
# 2. Duplicate lines, and in the total recorded land use area to be'/substantially greater than the recorded field area'/[May be a data entry error]'/
# 3. Land reported as LLO, and total recorded land use area to be'/substantially greater than the recorded field area'/[May be a seasonal tenancy incorrectly entered on permanent sheet]'/
# 4. On seasonal sheet, MLC and SLC are identical'/[May be a permanent tenancy incorrectly entered on seasonal sheet]'/
# 5.  The total recorded land use area is greater than the recorded'/field area by a factor of 10 or 100'/[May be a decimal place error]'/
# 6. SLC = 000/0000 on permanent land or MLC = 000/000 on seasonal land'/		- ie. parish/holding is unknown'/
# 7. FID recorded as belonging to multiple holdings (SLC) without any reported as seasonally let out (LLO).
# 8. Other land where total recorded land use area is substantially'/   greater than the recorded field area'/
# 9. EXCL land where total recorded land use area is substantially'/ greater than the recorded field area'/

# Limits for checking against

under_reportlimit <- 500
over_report_limit <- 5
under_report_percent <- 0.5
over_report_percent <- 1.1


# Permanent dataset - checks and error flags

# Check frequency of fids (field id), whether multiple holdings are using the same fid.


fidfreqsorig<-saf_perm %>% 
  select(slc, fid) 

# fidfreqsorig<-fidfreqsorig %>% 
#   add_row(slc="test/test", fid="049/0175/X")


fidfreqs<-fidfreqsorig %>% 
  group_by(fid) %>% 
  dplyr::summarise(count=n()) %>% 
  flatten() %>% 
  dplyr::rename(fid_uses_by_slc = count)

# Index by FID (create line variable)

fids<-fid_index(fidfreqsorig)

fids<-fids %>%
  filter(line==1) %>% 
  dplyr::rename(holdings_using_fid=line)

fidfreqsfinal<-merge(fidfreqs, fids, by=c("fid"))

fids_with_multiple_slcs<-fidfreqsfinal %>%
  filter(holdings_using_fid>1) # There are no fids with multiple slcs in the 2023 dataset. Same when using SAS code.




# Filter out LMC (Land Management Contract) claimtype. 
 
saf_perm_notlmc<-saf_perm %>% 
  filter(claimtype!="LMC")

fids_with_multiple_slcs<-merge(fids_with_multiple_slcs, saf_perm_notlmc, by=c("slc","fid"))


# FIDs with land let out (llo)

fids_with_llo<-saf_perm %>% 
  filter(llo=="Y") %>% 
  select(slc, fid) %>% 
  unique()



# Filter out LMC claimtype from this point.


fids_with_multiple_slcs <- fids_with_multiple_slcs[c('fid', 'slc')] # using indexing because filter doesn't work on an empty df


saf_perm<-plyr::join_all(list(saf_perm_notlmc, fids_with_multiple_slcs,fids_with_llo), by=c("slc","fid"), type='left')


saf_perm<-left_join(saf_perm, fids_with_llo, by=c("slc","fid"))


llo_error<-inner_join(saf_perm,fids_with_multiple_slcs,fids_with_llo, by=c("slc","fid"))


flag1<-llo_error %>% 
  filter(llo=="Y") %>%
  dplyr::mutate(flag1=1,
       flag7=0)


flag7<-inner_join(saf_perm,fids_with_multiple_slcs,by=c("slc","fid"))


flag7<-anti_join(flag7,flag1,by=c("slc","fid"))

flag7<-flag7 %>% 
  select(slc, fid) %>% 
  dplyr::mutate(flag1=0,
         flag7=1)



# merge flag 1 and flag7 here with saf_perm if they have rows in them. Currently they're empty so we can't. 


# Instead, create flag1 and flag7 in saf_perm

saf_perm<-saf_perm %>% 
  mutate(flag1=0,
         flag7=0)



# Dataframes for other error flags
# Select variables, group by fid and summarise

checkareasummary <- saf_perm %>%
  select(fid, area, field_area, eligible_area, land_use_area, flag1, flag7) %>% 
  group_by(fid) %>% 
  summarycheckarea()

# Dataframe for when field area is inconsistent. Currently 0 records. 

inconsistentfieldareas <- checkareasummary %>%
  filter(var_field > 0)

# Check for differences between total land use and field area (over the overreportlimit or overpercent which is coded at the top of the script)
# Currently 101 records (Data Drop 1: 107). 

checkareamismatches <- checkareasummary %>%
  mutateareamismatches() %>% 
  filter(diff > 500 | diff < (-5) | ratio > 1.1 | ratio < 0.5 & max_field > 0 & sum_area > 0 & sum(flag1) >= 0) %>% 
  select(fid, sum_area, max_field, diff, ratio)

# Fid level dataset for fids with a mismatch between land use area total and recorded field area
# Currently 189 records. (DD1: 176)

checkareamismatches_fids <- merge(checkareamismatches, saf_perm, by = "fid")


# check for decimal point (dp) errors. Currently 2 records (DD1:2). 

dperror <- merge(checkareasummary, saf_perm, by = "fid")

dperror <- dperror %>%
  mutate(dp_ratio = signif(area / (field_area - sum_area + area), 3)) %>% 
  filter(dp_ratio == 0.01 | dp_ratio == 0.1 | dp_ratio == 10 | dp_ratio == 100)



# Flag duplicates where total land use is substantially greater than field area - keeps SFPS over OTHER

# Currently 97 records (DD1:99).

areaoverreported <- checkareamismatches_fids %>%
  filter(ratio > 1.1 | diff < (-5)) %>% 
  mutate(
    across(claimtype, as_factor)
  )

# 97 records (DD1:78)

remove_duplicates <- areaoverreported %>%
  group_by(fid, area, code) %>%
  filter(!(claimtype == "OTHER" & n() > 1)) %>% 
  ungroup()

# 0 records (DD1:21)

duplicates <- areaoverreported %>%
  group_by(fid, area, code) %>%
  filter((claimtype == "OTHER" & n() > 1))




# flag EXCL land where total crop area is too large


areastilloverreported <- remove_duplicates %>%
  group_by(fid) %>% 
  dplyr::summarize(
    max_field = max(field_area),
    sum_area = sum(area))

areastilloverreported <- merge(areastilloverreported, saf_perm, by = "fid")

# Currently 92 records (DD1:70).

areastilloverreported <- areastilloverreported %>%
  mutate(
    diff = max_field - sum_area,
    ratio = signif(sum_area / max_field, 3)) %>% 
  filter(ratio > over_report_percent | diff < (-over_report_limit))

# Currently 24 records (DD1:17)

overreportedexclerror <- areastilloverreported %>%
  filter(code == "EXCL")

# Currently 68 records (DD1:53)

areastilloverreported2 <- areastilloverreported %>%
  filter(code != "EXCL")

# Currently 41 records (DD1:36).

areastilloverreported2 <- group_by(areastilloverreported2, fid) %>%
  dplyr::summarize(
    max_field = max(field_area),
    sum_area = sum(area)
  )



# Flag llo land where total land area is still greater than the field area by over_report amount (after accounting for duplicates). Land reported as seasonally let out as these are likely errors.


areastilloverreported2 <- merge(areastilloverreported2, saf_perm, by = "fid")

# 63 records (DD1:46)

areastilloverreported2 <- areastilloverreported2 %>%
  mutate(
    diff = max_field - sum_area,
    ratio = signif(sum_area / max_field, 3)) %>% 
  filter(ratio > over_report_percent | diff < (-over_report_limit))

# 0 records

overreportedlloerror <- areastilloverreported %>%
  filter(llo == "Y")

# 63 records

areastilloverreported3 <- areastilloverreported2 %>%
  filter(llo != "Y")

# 27 records (DD1:27) 

areastilloverreported3 <- group_by(areastilloverreported3, fid) %>%
  dplyr::summarize(
    max_field = max(field_area),
    sum_area = sum(area)
  )



# Flag records that may be errors

# 63 records (DD1:46)

areastilloverreported3 <- merge(areastilloverreported3, saf_perm, by = "fid") %>% 
  filter(llo != "Y") %>%
  mutate(
    diff = max_field - sum_area,
    ratio = signif(sum_area / max_field, 3)
  ) %>% 
  filter(ratio > over_report_percent)



# get unique llo errors

overreportedllofids <- overreportedlloerror %>%
  group_by(fid) %>%
  filter(!(n() > 1)) %>% 
  select(fid, mlc) %>%
  dplyr::rename(llomlc = mlc)


overreportedothererror <- merge(overreportedllofids, areastilloverreported3, by = "fid", all = TRUE)

# 41 records (DD1:28)

overreportedothererror <- overreportedothererror %>%
  filter(claimtype == "OTHER")%>% 
  mutate(llomlc=mlc)





# Create flag 6 (parish or holding unknown) in saf_perm (see above for flag descriptions)

saf_perm <- saf_perm %>%
  mutate(
    crops = code,
    flag6 =
      ifelse(parish <= 0 | holding <= 0, 1, 0)
  )


# Create flag 2 (duplicates)

duplicates <- duplicates %>%
  select(brn, fid, line, claimtype, code, area) %>% 
  mutate(flag2=1)

# Create flag 3 (overreported llo error). Currently df is empty.

overreportedlloerror <- overreportedlloerror %>% dplyr::mutate(flag3 = 1)

overreportedlloerror <- overreportedlloerror %>%
  select(brn, fid, line, claimtype, code, area) 

# Create flag 5 (decimal point error)

dperror <- dperror %>%
  select(brn, fid, line, claimtype, code, area, dp_ratio, sum_area) %>% 
  mutate(flag5=1)

# Create flag 8 (overreported other land error)

overreportedothererror <- overreportedothererror %>%
  select(brn, fid, line, claimtype, code, area, llomlc) %>% 
  mutate(flag8=1)

# Create flag 9 (overreported EXCL land error)

overreportedexclerror <- overreportedexclerror %>%
  select(brn, fid, line, claimtype, code, area) %>% 
  mutate(flag9=1)



# Create list of all dfs, including errors with their flags.

df_list <- list(saf_perm, duplicates, overreportedlloerror, dperror, overreportedothererror, overreportedexclerror)

# Merge all data frames in list, creating permanent dataset with all flags


finalsaf_perm <- df_list %>% reduce(full_join, by = c("brn", "fid", "line", "claimtype", "code", "area"))



# Seasonal dataset flagging 

# Flag fids which look like permanent lets (slc=mlc).

saf_seas <- saf_seas %>%
  mutate(
    flag4 =
      ifelse(mlc == slc, 1, 0)
  )


# In the SAS code, there is a "stage 4" here (commented out) which checks if multiple holdings use the same fids - this isn't as relevant as for permanent sheets - decide if we should bring this in or not?

# Flag duplicates where land use total substantially greater than field area

checkarea_seas <- saf_seas %>%
  filter(claimtype != "LMC")

checkarea_seas <- group_by(checkarea_seas, fid) %>%
  dplyr::summarize(
    sum_area = sum(area),
    sum_field = sum(field_area),
    sum_eligible = sum(eligible_area),
    max_field = max(field_area),
    var_field = var(field_area)
  )

# 0 records. 

inconsistentfieldareas_seas <- checkarea_seas %>%
  filter(var_field > 0)

# 426 records (DD1: 438)

checkareamismatches_seas <- checkarea_seas %>%
  filter(max_field > 0 & sum_area > 0) %>%
  mutate(
    diff = round_half_up(max_field - sum_area, 3),
    ratio = round_half_up(sum_area / max_field, 3)
  ) %>% 
  filter(diff > under_reportlimit | diff < (-over_report_limit) | ratio > over_report_percent | ratio < under_report_percent) %>% 
  select(fid, sum_area, max_field, diff, ratio)



# Create fid level dataset where land use area and field area don't match

# 501 records (DD1: 535) 

checkareamismatches_fids_seas <- merge(checkareamismatches_seas, saf_seas, by = "fid")


checkarea_seas <- checkarea_seas %>%
  select(fid, sum_area, max_field)


# Flag decimal point (dp) errors

dperror_seas <- merge(checkarea_seas, saf_seas, by = "fid")

# 1 record (DD1: 1)

dperror_seas <- dperror_seas %>%
  mutate(dp_ratio = signif(area / (field_area - sum_area + area), 3)) %>% 
  filter(dp_ratio == 0.01 | dp_ratio == 0.1 | dp_ratio == 10 | dp_ratio == 100)


# remove duplicates where total land use greater than field area by over report amount

# 89 records (DD1:125)

areaoverreported_seas <- checkareamismatches_fids_seas %>%
  filter(ratio > over_report_percent | diff < (-over_report_limit))

# order by Business Name

areaoverreported_seas <- areaoverreported_seas %>% arrange(business_name)

# 60 records (DD1:84)
 
remove_duplicates_seas <- areaoverreported_seas[!duplicated(areaoverreported_seas[c("fid", "area", "code")]), ]

# 29 records (DD1:41)

duplicates_seas <- areaoverreported_seas[duplicated(areaoverreported_seas[c("fid", "area", "code")]), ]


# Check fids where the claimed area is much larger than field area

areastilloverreported_seas <- group_by(remove_duplicates_seas, fid) %>%
  dplyr::summarize(
    max_field = max(field_area),
    sum_area = sum(area)) %>% 
  select(fid, max_field, sum_area)


areastilloverreported_seas <- merge(areastilloverreported_seas, saf_seas, by = "fid")

# 47 records (DD1:65)

areastilloverreported_seas <- areastilloverreported_seas %>%
  mutate(
    diff = max_field - sum_area,
    ratio = signif(sum_area / max_field, 3)) %>% 
  filter(ratio > over_report_percent | diff < (-over_report_limit))

# Create flag 6

saf_seas <- saf_seas %>%
  mutate(
    crops = code,
    flag6 =
      ifelse(parish <= 0 | holding <= 0, 1, 0)
  )


# Create flag 2

duplicates_seas <- duplicates_seas %>%
  select(brn, fid, line, claimtype, area, code) %>%
  mutate(flag2 = 1)

# Create flag 5

dperror_seas <- dperror_seas %>%
  select(brn, fid, line, claimtype, area, code, dp_ratio, sum_area) %>%
  mutate(flag5 = 1)

# Create list of all dfs, including errors with their flags.

df_list_seas <- list(saf_seas, duplicates_seas, dperror_seas)

# Merge all dfs in list, creating seasonal dataset with flags in

finalsaf_seas <- df_list_seas %>% reduce(full_join, by = c("brn", "fid", "line", "claimtype", "code", "area"))




# Additional checks and automatic corrections -------------------------------------------------

# From B8 section of SAS code

# rename dataframes

saf_permcurr<-finalsaf_perm
saf_seascurr<-finalsaf_seas

# Read in last year's data

saf_prev <- read_table_from_db(server=server, 
                               database=database, 
                               schema=schema, 
                               table_name="saf2022")  

# Reading in from ADM creates a new variable with id. Remove if necessary.

saf_prev<-saf_prev %>% 
  select(-any_of("allsaf22ID"))


# rename area in current seasonal data

saf_seascurr_fid <- saf_seascurr %>%
  dplyr::rename(areacurr = area)



# Split seasonal data into different LLIs

saf_seasprev_fid <- saf_prev %>%
  filter(substr(code, 1, 4) == "LLI-" & claimtype == "SFPS") %>%
  dplyr::rename(areaprev = area) %>% 
  distinct(parish, holding, fid, .keep_all = TRUE) %>% 
  filter(!(is.na(parish) | is.na(holding)))


saf_seasprev_cph <- saf_prev %>%
  select(parish, holding) %>% 
  filter(!(is.na(parish) | is.na(holding)))

# remove duplicates

saf_seasprev_cph <- saf_seasprev_cph %>%
  distinct(parish, holding, .keep_all = TRUE) %>% 
  mutate(
    parish = as.numeric(parish),
    holding = as.numeric(holding)
  )

saf_seascurr_fid <- saf_seascurr_fid %>% 
  mutate(
    parish = as.numeric(parish),
    holding = as.numeric(holding)) %>% 
  filter(!(is.na(parish) | is.na(holding) | is.na(fid))) # This isn't in SAS but necessary to make later code work




# Different LLI types are assigned here.

# 	- LLI-SL = Land seasonally let in at same location as last year
# - LLI-DL = Land seasonally let in at a different location to last year
# - LLI-NL = Land seasonally let in but no SAF claimed last year

seascurr_matched <- merge(saf_seascurr_fid, saf_seasprev_cph, by = c("parish", "holding"))


onlycurr_fid <- setdiff(saf_seascurr_fid, seascurr_matched)

saf_seasprev_fid2 <- saf_seasprev_fid %>%
  select(areaprev, parish, holding, fid)

bothyears <- merge(seascurr_matched, saf_seasprev_fid2, by = c("parish", "holding", "fid"), all.x = TRUE)



bothyears <- bothyears %>%
  mutate(
    code =
      ifelse(areacurr > 0 & areaprev > 0, "LLI-SL", "LLI-DL")
  ) %>% 
  select(-areaprev)

onlycurr_fid <- onlycurr_fid %>%
  mutate(code = "LLI-NL")


split <- rbind(bothyears, onlycurr_fid)


pfdscurr_seas <- split %>%
  dplyr::rename(area = areacurr) %>%
  mutate(
    llo = 0,
    landtype = "SEAS",
    claimtype = "SFPS"
  )




# Automatic corrections ---------------------------------------------------


# Remove flagged entries here if required  (SAF validations also in C)

cols<-as.data.frame(compare_df_cols(saf_permcurr, pfdscurr_seas))

pfdscurr_seas<-pfdscurr_seas %>% 
  mutate( code= as.factor(code),
          llo=as.character(llo))

pfds_finalcurr <- bind_rows(saf_permcurr, pfdscurr_seas)


pfds_corrections1 <- pfds_finalcurr %>%
  filter(flag6 == 1 & !is.na(mlc) & landtype == "PERM") %>%
  dplyr::mutate(
    parish = str_remove(substr(mlc, 1, 3), "^0+"),
    holding = str_remove(substr(mlc, 5, 8), "^0+"),
    flag6 = 0
  )


pfds_corrections2 <- pfds_finalcurr %>%
  filter((flag1 > 0 | flag2 > 0) & !is.na(fid))


pfds_corrections3 <- pfds_finalcurr %>%
  filter(flag5 > 0 & !is.na(fid))



pfdscorrections <- rbind(pfds_corrections1, pfds_corrections2, pfds_corrections3)

# Following chunk only works when flag3 is present.

# pfds_finalcurr<-pfds_finalcurr %>%
#   filter(!flag3>0)


pfds_finalcurr <- setdiff(pfds_finalcurr, pfds_corrections2)


pfds_finalcurr <- pfds_finalcurr %>%
  mutate(
    area =
      ifelse(flag5 > 0 & !is.na(flag5), (area / dp_ratio), area),
    flag5 = 0,
    flag6 = 0
  )



# create permanent and seasonal datasets if necessary

saf_permcurr <- pfds_finalcurr %>%
  filter(landtype == "PERM")


saf_seascurr <- pfds_finalcurr %>%
  filter(landtype == "SEAS")


saf_permseas<-rbind(saf_permcurr,saf_seascurr)

# Keep necessary variables of combined dataset

saf_curr <- pfds_finalcurr %>%
  select(-c(business_name, land_use, land_use_area, bps_claimed_area, application_status, is_perm_flag, sfp_area, sfp_code, dp_ratio, sum_area))

# Save corrections and combined allsaf flagged dataset to datashare

save(saf_curr, file = paste0(Code_directory, "/allsaf_B8_2023.rda"))

save(pfds_finalcurr, file = paste0(Code_directory, "/allsaf_B8flags_2023.rda"))

save(saf_permseas, file = paste0(Code_directory, "/allsaf_B8permseas_2023.rda"))

save(pfdscorrections, file = paste0(Code_directory, "/allsaf_B8corrections_2023.rda"))

# Save to ADM server

write_dataframe_to_db(server=server,
                      database=database,
                      schema=schema,
                      table_name="allsaf_B8_2023",
                      dataframe=saf_curr,
                      append_to_existing = FALSE,
                      versioned_table=FALSE,
                      batch_size = 10000)


write_dataframe_to_db(server=server,
                      database=database,
                      schema=schema,
                      table_name="allsaf_B8flags_2023",
                      dataframe=pfds_finalcurr,
                      append_to_existing = FALSE,
                      versioned_table=FALSE,
                      batch_size = 10000)

write_dataframe_to_db(server=server,
                      database=database,
                      schema=schema,
                      table_name="allsaf_B8permseas_2023",
                      dataframe=saf_permseas,
                      append_to_existing = FALSE,
                      versioned_table=FALSE,
                      batch_size = 10000)


write_dataframe_to_db(server=server,
                      database=database,
                      schema=schema,
                      table_name="allsaf_B8corrections_2023",
                      dataframe=pfdscorrections,
                      append_to_existing = FALSE,
                      versioned_table=FALSE,
                      batch_size = 10000)




# EXCL apportioning -------------------------------------------------------

# From B8b of SAS project


saf2015orig<-saf2015


#levels(as.factor(saf2015$code))

saf2015<-saf2015 %>% 
  clean_names() %>% 
  filter(landtype=="PERM") %>% 
    dplyr::rename(
    field_area15 = field_area,
    eligible_area15 = eligible_area,
    brn15 = brn,
    mlc15 = mlc,
    slc15 = slc ) %>% 
  dplyr::mutate(rgr15=ifelse((code=="RGR" | code =="IFL"), area, ""),
         woodland15=ifelse((code=="EX-SS"|code=="NETR-A"|code=="NETR-NA"|code=="TREES"|code=="WDG"), area, ""),
         otherland15=ifelse((code=="BRA"|code=="BUI"|code=="FSE"|code=="GOR"|code=="MAR"|code=="PRSL"|code=="ROAD"|code=="ROK"|code=="SCB"|code=="SCE"|code=="SCR"|code=="UNSP"|code=="WAT"|code=="OTH-LAND"), area, "")) %>%
  select(brn15, mlc15, slc15, field_area15,eligible_area15,rgr15,woodland15,otherland15,parish,holding,fid) %>% 
  mutate_at(c('field_area15', 'eligible_area15', 'rgr15', 'woodland15', 'otherland15'), as.numeric)

saf2015<-saf2015 %>% 
  group_by(parish, holding, fid) %>% 
  dplyr::summarise(brn15=max(brn15), slc15=max(slc15), mlc15=max(mlc15), field_area15=max(field_area15, na.rm=TRUE), eligible_area15=max(eligible_area15, na.rm=TRUE), rgr15=sum(rgr15, na.rm=TRUE), woodland15=sum(woodland15, na.rm=TRUE), otherland15=sum(otherland15, na.rm=TRUE), .groups="keep") %>% 
  ungroup()
saf2015<-as.data.frame(saf2015)

saf2015[saf2015 == ""] <- NA 

saf2015<-saf2015 %>% 
  filter_at(vars(parish, holding, fid),all_vars(!is.na(.)))

# Compare with SAS dataset

# saf2015SAS<-read.csv(paste0(Code_directory, "/SAF2015SAS.csv"))
# 
# saf2015SAS<-saf2015SAS %>% 
#   select(-X_TYPE_,-X_FREQ_)
# 
# saf2015SAS<-clean_names(saf2015SAS)
# 
# saf2015SAS<-saf2015SAS %>% 
#   mutate_at(c('parish', 'holding', 'brn15'), as.numeric) %>% 
#   mutate_if(is.numeric, ~replace_na(.,0))
# 
# saf2015SAS[saf2015SAS == ""] <- NA 
# 
# checkSAS<-intersect(saf2015,saf2015SAS)
# 
# checkSAS<-anti_join(saf2015,saf2015SAS)
# 
# checkR<-setdiff(saf2015SAS,saf2015)


saf2023<-saf_curr %>% 
  clean_names() %>% 
  filter(landtype=="PERM") %>% 
  dplyr::rename(
    field_area23 = field_area,
    eligible_area23 = eligible_area,
    brn23 = brn,
    mlc23 = mlc,
    slc23 = slc ) %>% 
  dplyr::mutate(rgr23=ifelse((code=="RGR" | code =="IFL"), area, ""),
         woodland23=ifelse((code=="EX-SS"|code=="NETR-A"|code=="NETR-NA"|code=="TREE"|code=="WDG"), area, ""),
         otherland23=ifelse((code=="BRA"|code=="BUI"|code=="FSE"|code=="GOR"|code=="MAR"|code=="PRSL"|code=="ROAD"|code=="ROK"|code=="SCB"|code=="SCE"|code=="SCR"|code=="UNSP"|code=="WAT"|code=="OTH-LAND"), area, "")) %>%
  select(brn23, mlc23, slc23, field_area23,eligible_area23,rgr23,woodland23,otherland23,parish,holding,fid) %>% 
  mutate_at(c('field_area23', 'eligible_area23', 'rgr23', 'woodland23', 'otherland23'), as.numeric)


saf2023<-saf2023 %>% 
  group_by(parish, holding, fid) %>% 
  dplyr::summarise(brn23=max(brn23), slc23=max(slc23), mlc23=max(mlc23), field_area23=max(field_area23, na.rm=TRUE), eligible_area23=max(eligible_area23, na.rm=TRUE), rgr23=sum(rgr23, na.rm=TRUE), woodland23=sum(woodland23, na.rm=TRUE), otherland23=sum(otherland23, na.rm=TRUE), .groups="keep") %>% 
  ungroup()
saf2023<-as.data.frame(saf2023)


excl_all<-saf_curr %>% 
  filter(code=="EXCL" & area >0 & landtype =="PERM")


excl<-excl_all %>% 
  group_by(parish, holding, fid) %>% 
  dplyr::summarise(area=sum(area), .groups="keep") %>% 
  ungroup()

check_excl<-inner_join(saf2015,excl, by=c("parish", "holding", "fid"))


check_excl<-left_join(check_excl, saf2023, by=c("parish", "holding", "fid"))

# compare with SAS

# check_exclSAS<-read.csv(paste0(Code_directory, "/CHECK_EXCLSAS.csv"))


newcode<-check_excl %>% 
  dplyr::mutate(
    newcode=
      ifelse(abs(area+otherland23-otherland15) < 0.01, "OTH-LAND",
             ifelse(abs(area+woodland23-woodland15) < 0.01, "TREE",
                    ifelse(abs(area+rgr23-rgr15) < 0.01, "RGR",
                            ifelse((area+otherland23) < otherland15, "OTH-LAND",
                               ifelse((area+woodland23) < woodland15, "TREE",
                                 ifelse((area+rgr23) < rgr15, "RGR", "EXCL")))))),
    newarea=area)


unmatched<-newcode %>% 
  select(parish, holding, fid, newcode, newarea, rgr23, rgr15, otherland23, otherland15, woodland23, woodland15) %>% 
  filter(newcode=="EXCL")

# save unmatched to ADM for use in B2


write_dataframe_to_db(server=server,
                      database=database,
                      schema=schema,
                      table_name="unmatched_EXCL_SAF_B1",
                      dataframe=unmatched,
                      append_to_existing = FALSE,
                      versioned_table=FALSE,
                      batch_size = 10000)


matched<-newcode %>% 
  select(parish, holding, fid, newcode, newarea, rgr23, rgr15, otherland23, otherland15, woodland23, woodland15) %>% 
  filter(newcode!="EXCL")


excl_all<-excl_all %>% 
  select(parish, holding, fid, line, code, area)

allmatched<-left_join(matched, excl_all, by=c("parish", "holding", "fid"))

allmatched<-allmatched %>% 
  mutate(
    code=newcode,
    crops=newcode
  )

beforeEXCLcorrection<-saf_curr %>% 
  group_by(code) %>% 
  summarise(area=sum(area))


allmatched<-unique(allmatched)

allmatched<-allmatched %>% 
  select(parish, holding, fid, line, area, code, crops) %>% 
  mutate(code=as.factor(code), 
         crops=as.factor(crops))

allmatched$code<-factor(allmatched$code, levels=union(levels(allmatched$code), levels(saf_curr$code)))
allmatched$crops<-factor(allmatched$crops, levels=union(levels(allmatched$crops), levels(saf_curr$crops)))

saf_curr$code<-factor(saf_curr$code, levels=union(levels(saf_curr$code), levels(allmatched$code)))
saf_curr$crops<-factor(saf_curr$crops, levels=union(levels(saf_curr$crops), levels(allmatched$crops)))

allsaf <- rows_update(saf_curr, allmatched, by = c("parish", "holding", "fid", "line"))


afterEXCLcorrection<-allsaf %>% 
  group_by(code) %>% 
  summarise(area=sum(area))





# Creating census-format dataset --------------------------------------------------

# B9 Section of SAS code 

#saf_curr<-loadRData(paste0(Code_directory, "/allsaf_B8_2023.rda"))

# Rename SAF df


# Aggregate data 

check.llo <- allsaf %>%
  filter(!(llo == "N" | llo == "Y"))


allsaf_fids <- allsaf %>%
  filter(!claimtype == "LMC") %>%
  mutatellolfass()%>% 
  group_by (parish, holding, fid, code) %>%
  summaryfids()



aggregate1 <- allsaf_fids %>%
  group_by(parish, holding, code) %>%
  aggregatefids()

allsaf_reduced <- allsaf %>%
  select(parish, holding, mlc, brn, area)

rm(allsaf_fids)




# Map SAF code to JAC item numbers 

# Translate codes to June items based on translation table (this will probably be updated every year)

aggregate1$code <- as.factor(aggregate1$code)
newcodetrans$code <- as.factor(newcodetrans$code)

cens_coded <- merge(aggregate1, newcodetrans, by = "code", all.x = TRUE)

unmatched_codes <-
  cens_coded [!aggregate1$code %in% cens_coded$code, ]


allsaf <- as_tibble(allsaf)



# Produces item185 for item 41 (item41 is Unspecified Crops Total Area). Item185 will specify the crops.


extra_ncode <- cens_coded %>%
  select(parish, holding, cens_code, code)

extra_ncode$item185 <- ""

extra_ncode <- extra_ncode %>%
  group_by(parish, holding) %>%
  dplyr::mutate(
    item185 =
      ifelse(row_number() == 1, "a", "")
  )


# Some of these will probably change yearly - if so, update othercropscodes in Functions script.

extra_ncode <- extra_ncode %>%
  group_by(parish, holding) %>%
  othercropscodes()

extra_ncode <- extra_ncode %>%
  group_by(parish, holding) %>%
  ncode()

extra_ncode$item185 [is.na(extra_ncode$item185)] <- ""

gc()

# Group by parish and holding. item185 consists of multiple strings concatenated - should change this to include semicolon (if collapse=";" it ends up with lots of unwanted semicolons!)

extra_item185 <- extra_ncode %>%
  group_by(parish, holding) %>%
  dplyr::summarise(item185 = paste(item185, collapse = "")) %>% 
  mutate(
    parish = as.numeric(parish),
    holding = as.numeric(holding)
  )  # On the run-through on 26.09.23 R gave an error at this point Error in stopifnot(is.character(filename), length(filename) == 1L) : reached elapsed time limit
# 

# Group by SLC

census_format <- cens_coded %>%
  group_by(parish, holding, cens_code) %>%
  censusformat()


# Extra fields to fold in later

extra_fields <- census_format %>%
  group_by(parish, holding) %>%
  extrafields()


#brns dataset

allsaf<-allsaf %>% 
  drop_na(c(parish, holding, brn, mlc, area))

brns <- allsaf %>%
  select(parish, holding, brn, mlc, area) %>%
  brnmutate() %>%  
  group_by(parish, holding, brn) %>%
  brnsummary() %>% 
  distinct(parish, holding, .keep_all=TRUE) 


# Reformat dataset

# Change dataset from long to wide

cens_wide <- census_format %>%
  select(parish, holding, cens_code, area) %>% 
  group_by(parish, holding) %>%
  pivot_wider(names_from = cens_code, values_from = area)



# Check where these items are in the JAC

order <-
  c(
    "parish",
    "holding",
    "item2321",
    "item2322",
    "item2828",
    "item9999",
    "item2469",
    "item2470",
    "item3156",
    "item47",
    "item20",
    "item16",
    "item14",
    "item19",
    "item41",
    "item70",
    "item48",
    "item18",
    "item2320",
    "item66",
    "item17",
    "item24",
    "item2827",
    "item34",
    "item29",
    "item32",
    "item49",
    "item31",
    "item30",
    "item28",
    "item52",
    "item1710",
    "item83",
    "item53",
    "item63",
    "item75",
    "item27",
    "item56",
    "item2858",
    "item82",
    "item80",
    "item2879",
    "item21",
    "item23",
    "item15",
    "item36",
    "item2059",
    "item2323",
    "item64",
    "item71",
    "item72",
    "item2859",
    "item2324",
    "item1709",
    "item2860",
    "item65",
    "item60",
    "item59",
    "item2832",
    "item61",
    "item2034",
    "item2861",
    "item55",
    "item81",
    "item2707",
    "item2863",
    "item2864",
    "item2865"
  )





orderdf <- order[order %in% colnames(cens_wide)]

cens_wide <- setDT(cens_wide)
cens_wide <- setcolorder(cens_wide, as.character(orderdf))



# add column not in dataset and reorder to include

addtodf <- order[!order %in% colnames(cens_wide)]

cens_wide[, addtodf] <- NA

cens_wide <- setcolorder(cens_wide, as.character(order))

# Convert any missing values, NULL, NA, to zeroes

cens_wide[cens_wide == "NULL"] <- 0

cens_wide[is.na(cens_wide)] <- 0





cens_wide<-data.frame(cens_wide) %>% 
  mutate_all(
    unlist(as.character)
  ) %>% 
  mutate_if(is.character, as.numeric) %>% 
  filter(!parish < 1 | !holding < 1)



# Prepare dfs for creating final SAF dataset

extra_fields <- extra_fields %>%
  select(parish, holding, lfass_area, llo_area) %>% 
  mutate(
    parish=as.numeric(parish),
    holding=as.numeric(holding)
  )

# remove leading zeroes

brns <- brns %>%
  select(parish, holding, brn, mlc) %>% 
  mutate(
    parish= as.numeric(sub("^0+", "", parish)),
    holding= as.numeric(sub("^0+", "", holding)),
  )


list<-list(cens_wide,extra_fields,extra_item185,brns)

cens_wide_final<-list %>% reduce(left_join, by = c("parish","holding"))



cens_wide_final <- cens_wide_final %>%
  newitemssaf()  




# Check what order the variables should be in - does it matter.

cens_wide_final[cens_wide_final == "NULL"] <- 0
cens_wide_final[is.na(cens_wide_final)] <- 0

# Remove any stray duplicates.

cens_wide_dups <- cens_wide_final[duplicated(cens_wide_final[, 1:2]), ]
cens_wide_final <- cens_wide_final[!duplicated(cens_wide_final[, 1:2]), ]



# Note: decide what checks to add here. e.g. SAS code checks if item50 exists (total land) and is > 0 in any cases. Item50 is not in the dataset.

# Save to datashare

save(cens_wide_final, file = paste0(Code_directory, "/allsaf_final_2023.rda"))

# Save to ADM server


write_dataframe_to_db(server=server,
                      database=database,
                      schema=schema,
                      table_name="allsaf_final_2023",
                      dataframe=cens_wide_final,
                      append_to_existing = FALSE,
                      versioned_table=FALSE,
                      batch_size = 10000)

