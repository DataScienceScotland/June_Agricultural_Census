# This script collates the SAF data supplied by RPID - code is rewrite of A1 section of the June SAS code (\\s0177a\datashare\seerad\ags\census\branch1\NewStructure\Surveys\June\Main\JUNE CENSUS PROJECT - 2021 Provisional Scott)
# Created by Lucy Nevard 22.11.22 
# Modified by Lucy Nevard 22.12.22



# Before import -----------------------------------------------------------


# Clear environment prior 

rm(list=ls())

# Set working directory


setwd("//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Main/June21/Data Drops/SAF")

# Load packages

library(readxl)
library(data.table)
library(dplyr)
library(stringr)


#  Assign year and month to allow creation of dataset with correct year (at the very end of this script). 
# Depending on how data are stored, year/month may need to be specified in readxl

yr<-21
mnth<-"SEP"



# Import ------------------------------------------------------------------


# Import datasets separately (or do this directly into a list depending on what other files are in the folder).

df_permanent<-read_excel("permanent_output1_SecondDrop.xlsx")  # called script1 in SAS
df_scheme<-read_excel("scheme_output1_SecondDrop.xlsx")  # called script2 in SAS
df_seasonal<-read_excel("seasonal_output1_SecondDrop.xlsx") # called script3 in SAS




# Create list and index by LPID -------------------------------------------


# Create list for permanent and seasonal sheets - keep scheme separate as it has different variables. Is there a better way to work with them all in a list?

list_perm_seas<-list(df_permanent, df_seasonal)

# Create line variable for list to index by LPID for permanent and seasonal. Number gives instance of each LPID. 

list_perm_seas<-lapply(list_perm_seas, function(x)
  
  {setDT(x)[,line:=seq(1,.N),by=LPID]})

# Create new df and line variable for scheme

df_schemework<-setDT(df_scheme)[,line:=seq(1,.N),by=LPID]




# Rename and create variables ---------------------------------------------

# Rename and create new variables for permanent and seasonal. Land Use is also renamed here for ease, as are landusearea and bpsclaimedarea.


list_perm_seas<-lapply(list_perm_seas, function(x)
  
{rename(x, 
        brn = "Business Reference Number",
        mlc = "Main Location Code",
        slc = "Location Code",
        fid = LPID,
        field_area = "Land Parcel Area",
        lfass_eligible = LFASS,
        eligible_area = "BPS Eligible Area",
        LLO = "Land Leased Out",
        land_use = "Land Use",
        land_use_area = "Land Use Area",
        bps_claimed_area = "BPS Claimed Area"
)})



list_perm_seas<-lapply(list_perm_seas, function(x)

  {mutate(x,
    slc = substr(slc,4,11),
    mlc = substr(mlc,4,11),
    parish = str_remove(substr(slc,1,3), "^0+"),
    holding = str_remove(substr(slc,5,8),"^0+"),
    land_use = str_replace_all(land_use, "_", "-"),
    sfp_area = bps_claimed_area,
    sfp_code = land_use,
    other_area = (land_use_area-bps_claimed_area),
    other_code = land_use
  )})


# rename and create new variables in scheme.

df_schemework<-df_schemework %>%
  rename(
    brn = "Business Reference Number",
    mlc = "Main Location Code",
    slc = "Location Code",
    fid = LPID,
  )


df_schemework<-df_schemework%>%
  mutate(
    slc = substr(slc,4,11),
    mlc = substr(mlc,4,11),
    parish = str_remove(substr(slc,1,3), "^0+"),
    holding = str_remove(substr(slc,5,8),"^0+"),
    SCHEME_OPTION_CODE = str_replace_all(SCHEME_OPTION_CODE, "_", "-") # not sure this line is necessary - there are no underscores or hyphens?
  )




# Create dfs for missing obs ----------------------------------------------

# Create dfs for missing observations in permanent and seasonal. 
# Note: this is looking for NAs rather than full stops or blanks as in the SAS code. R gives blanks as NAs, unlike in SAS. 

missingperm_seas<-lapply(list_perm_seas, function(x)
  
  {subset(x,
          is.na(brn) == TRUE | is.na(mlc) == TRUE | is.na(line) == TRUE | is.na(slc) == TRUE | is.na(fid) ==TRUE
          )})


# Missing field area in permanent and seasonal. Again, we are looking for NAs and 0s (rather than full stops and 0s as in SAS).

missfieldareaperm_seas<-lapply(list_perm_seas, function(x)
  
{x[field_area==0 | is.na(field_area)]
  })


# Missing eligible area in permanent and seasonal.

misseligibleareaperm_seas<-lapply(list_perm_seas, function(x)
  
{x[is.na(eligible_area)]
})


# Write these to rda (for future use)
# 
# setwd("C:/Users/u455049/Documents/R/repos/June")
# 
# save(missingperm_seas, file=paste("missingallsaf",yr, "perm_seas", mnth, sep='_',".rda"))
# save(missfieldareaperm_seas, file=paste("missingfieldareaallsaf",yr, "perm_seas", mnth, sep='_',".rda"))
# save(misseligibleareaperm_seas, file=paste("missingeligibleareaallsaf",yr, "perm_seas", mnth, sep='_',".rda"))


# Create final dataframes -------------------------------------------------

# final list of permanent and seasonal removing blank records. 

list_perm_seas<-lapply(list_perm_seas, function(x)
  
{x[!is.na(brn) | ! is.na(mlc) | !is.na(slc)]
})


# final scheme df removing blank records. 


df_schemework<-df_schemework[!is.na(brn) | ! is.na(mlc) | !is.na(slc)]


#  Create final dfs for permanent, scheme and seasonal. 

# Unlist permanent and seaonal dfs (creating df1 and df2). Ideally should do this combining the following lines in one step...

for (i in seq(list_perm_seas))
   assign(paste0("df", i), list_perm_seas[[i]])


# rename df with correct year


# This is where the A1 section of the SAS code ends.

# Write to rda with name including year and month. Figure out how to take out the last underscore...

setwd("C:/Users/u455049/Documents/R/repos/June")

save(df1, file=paste("allsaf",yr, "perm", mnth, sep='_',"A1end.rda"))
save(df2, file=paste("allsaf",yr, "seas", mnth, sep='_',"A1end.rda"))
save(df_schemework, file=paste("allsaf",yr, "scheme", mnth, sep='_',"A1end.rda"))


# Write to csv if necessary ------------------------------------------------------------


# Write dfs to csv 
# Note: year and month are specified in the object name already...
# 
# setwd("C:/Users/u455049/Documents/R/repos/June")
# 
# 
# write.csv(allsaf_21_perm_SEP, "allsaf_21_perm_SEP.csv")
# write.csv(allsaf_21_scheme_SEP, "allsaf_21_scheme_SEP.csv")
# write.csv(allsaf_21_seas_SEP, "allsaf_21_seas_SEP.csv")






