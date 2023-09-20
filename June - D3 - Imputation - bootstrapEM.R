# This script takes in the pre-imputation dataset rolled forward dataset and performs multiple imputation for imputed items. 
# There are two D3 scripts. This one is for the function bootstrapEM. The other is for the more time-consuming function chainedEQ. Only one will be run in the final cut.
# Created by Lucy Nevard June 2023. 
# Modified by Lucy Nevard 07.09.23




# Before import -----------------------------------------------------------


# If you need to install imputeJAS, Move the imputejas-main.zip ("//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Codeconversion_2023/BIOSS) into your TEMP folder and run the installation code below. There is also a sample script in the same folder.


# remotes::install_local("C:/temp/imputejas-main.zip",
#                        type="source", repos=NULL)



rm(list=ls())

library(dplyr)
library(imputeJAS)
library(RtoSQLServer)
library(janitor)
library(stringr)


source("Functions/Functions.R")

Code_directory <- ("//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Codeconversion_2023/2023")

server <- "s0196a\\ADM"
database <- "RuralAndEnvironmentalScienceFarmingStatistics"
schema <- "juneagriculturalsurvey2023alpha"


# Import ------------------------------------------------------------------


# Load pre_imputation dataset

load(paste0(Code_directory, "/pre_imputation_rolled_forward.rda"))


# pre_imputation_rolled_forward is a large dataset and is time-consuming to read in from ADM - read in from the datashare instead if needed. 

# pre_imputation_rolled_forward<- read_table_from_db(server=server, 
#                                      database=database, 
#                                      schema=schema, 
#                                      table_name="pre_imputation_rolled_forward")



# Prepare for imputation -----------------------------------------------------

# These are the imputed item. This list may be updated if decided by the Census team. 

imputed_items<-c("item14","item24","item94","item139","item140","item141","item143","item144","item145","item2038","item2039",
                 "item2320","item2321","item2322","item2469","item2470","item2472","item2473","item2474","item2862","item2868",
                 "item27710","item27715","item27720","item27725","item27730","item27735","item27740","item27750",
                 "item27755","item27775","item27780") 

# Keep only imputed items in the dataset

pre_imputation_reduced<-pre_imputation_rolled_forward %>% 
  select(c("yr","id","parish","holding", "ags_madeup", "saf_madeup"),all_of(imputed_items))

# Check 2023 data to see if it looks sensible

check2023full<-pre_imputation_reduced %>% filter(yr=="2023")


pre_imputation_reduced<-as.data.frame(pre_imputation_reduced)

# Add zeroes for items when 2023 is NA and all historic data are zero

pre_imputation_reduced_zeroes<-create_zeroes(pre_imputation_reduced)

# Add zeroes when ags_madeup >=10 and saf_madeup>=10 

madeupoverten<-create_df_madeup(pre_imputation_reduced_zeroes)

madeupoverten<-create_zeroes_madeup(madeupoverten)

# Recreate dataset with zeroes

pre_imputation_reduced_zeroes<-rows_update(pre_imputation_reduced_zeroes, madeupoverten, by=c("id", "yr"))

# Save this so that the imputed items can be added back to it in D4

save(pre_imputation_reduced_zeroes, file = paste0(Code_directory, "/pre_imputation_reduced_zeroes.rda"))

# Keep holdings only when 2023 has NAs. This should reduce the size of the dataset significantly, .e.g end up with ~270000 rows (as of 20/09/23, this will be fewer as we receive more census responses)

pre_imputation_final <- keep_missing(pre_imputation_reduced_zeroes)

pre_imputation_final<-as.data.frame(pre_imputation_final)


str(pre_imputation_final)


check2023<-pre_imputation_final %>% filter(yr=="2023")

# Make sure id is a factor - imputation doesn't work properly otherwise

pre_imputation_final$id<-factor(pre_imputation_final$id)


# Subset data  ------------------------------------------------------------

# Data needs to be subsetted to allow imputation to run without breaking

list_subsets<-list(
  pre_imputation_final %>%
    filter(str_starts(id, '1')|str_starts(id, '2')|str_starts(id, '3')|str_starts(id, '4')) %>%
  select(c("yr","id"),5:12),
  pre_imputation_final %>%
    filter(str_starts(id, '1')|str_starts(id, '2')|str_starts(id, '3')|str_starts(id, '4')) %>%
    select(c("yr","id"),13:20),
  pre_imputation_final %>%
    filter(str_starts(id, '1')|str_starts(id, '2')|str_starts(id, '3')|str_starts(id, '4')) %>%
    select(c("yr","id"),21:28),
  pre_imputation_final %>%
    filter(str_starts(id, '1')|str_starts(id, '2')|str_starts(id, '3')|str_starts(id, '4')) %>%
    select(c("yr","id"),29:36),
  pre_imputation_final %>%
    filter(str_starts(id, '5')|str_starts(id, '6')|str_starts(id, '7')|str_starts(id, '8')|str_starts(id, '9')) %>%
    select(c("yr","id"),5:12),
  pre_imputation_final %>%
    filter(str_starts(id, '5')|str_starts(id, '6')|str_starts(id, '7')|str_starts(id, '8')|str_starts(id, '9')) %>%
    select(c("yr","id"),13:20),
  pre_imputation_final %>%
    filter(str_starts(id, '5')|str_starts(id, '6')|str_starts(id, '7')|str_starts(id, '8')|str_starts(id, '9')) %>%
    select(c("yr","id"),21:28),
  pre_imputation_final %>%
    filter(str_starts(id, '5')|str_starts(id, '6')|str_starts(id, '7')|str_starts(id, '8')|str_starts(id, '9')) %>%
    select(c("yr","id"),29:36)
)


# Change variables to upper case

list_subsets<-lapply(list_subsets, function (x) dplyr::rename_with(x, toupper))


# Keep only levels of factor which exist in each dataframe

list_subsets<-lapply(list_subsets, function(x){x$ID <- factor(x$ID,); x})

str(list_subsets[3])

# Change row numbers to 1->x for each dataframe

list_subsets<-lapply(list_subsets, function(x) {x<-as.data.frame(x); x})

list_subsets<-lapply(list_subsets, function(x) {rownames(x)<-1:nrow(x); x})

str(list_subsets[3])

rownames(list_subsets[[3]])

# Remove large datasets and clean up memory 
rm(pre_imputation_rolled_forward, pre_imputation_reduced)

gc()




# Run imputation ----------------------------------------------------------

# bootstrapEM is the imputeJAS function created by BIOSS, which wraps around Amelia. 
# Running the following should take <2h with the current dataset (06.09.23)
# m indicates the number of imputations run for each item (if NA) within each holding

old <- Sys.time()
list_jacs_imputed<-pbapply::pblapply(list_subsets, function (x) bootstrapEM(x, ts = "YR", cs = "ID", m = 20)) # pbapply shows a progress bar - this will update after every 12.5% (i.e. for each 8 dfs in the list)
new <- Sys.time() - old 
print(new)

# The output above is a huge file - we only need the 2023 results

outputs<-lapply(
  list_jacs_imputed, function (x) filter(x$results, YR == max(unique(YR), na.rm=TRUE)))


# Save outputs ------------------------------------------------------------



# Save these for use in D4. Change date as needed. 
# They are currently a list, so need to be processed further before saving in ADM at this point. Will add this in the future.

save(outputs, file = paste0(Code_directory, "/jac_bootstrapEM_060923.rda"))

