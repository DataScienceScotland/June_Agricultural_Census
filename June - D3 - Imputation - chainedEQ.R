# This script takes in the pre-imputation dataset rolled forward dataset and performs multiple imputation for imputed items. 
# There are two D3 scripts. This one is for the function chaineEQ. The other is for the less time-consuming function bootstrapEM. Only one will be run in the final cut.
# Created by Lucy Nevard June 2023. 
# Modified by Lucy Nevard 07.09.23


# Before import -----------------------------------------------------------


# If you need to install imputeJAS, Move the imputejas-main.zip ("//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Codeconversion_2023/BIOSS) into your TEMP folder and run the installation code below. There is also a sample script in the same folder.


# remotes::install_local("C:/temp/imputejas-main.zip",
#                        type="source", repos=NULL)



rm(list=ls())


library(dplyr)
library(RtoSQLServer)
library(janitor)
library(stringr)
library(imputeJAS)


#source("Functions/Functions.R")

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



# Multiple imputation -----------------------------------------------------

# These are the imputed item. This list may be updated if decided by the Census team. 

imputed_items<-c("item14","item24","item94","item139","item140","item141","item143","item144","item145","item2038","item2039",
                 "item2320","item2321","item2322","item2469","item2470","item2472","item2473","item2474","item2862","item2868",
                 "item27710","item27715","item27720","item27725","item27730","item27735","item27740","item27750",
                 "item27755","item27775","item27780","item146", "item147","item148", "item149","item150", "item151") 


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

# Keep holdings only when 2023 has NAs. This should reduce the size of the dataset significantly, .e.g end up with ~321000 rows (as of 06/09/23, will be fewer as we receive more census responses)

pre_imputation_final <- keep_missing(pre_imputation_reduced_zeroes)

pre_imputation_final<-as.data.frame(pre_imputation_final)


str(pre_imputation_final)


check2023<-pre_imputation_final %>% filter(yr=="2023")

# Make sure id is a factor - imputation doesn't work properly otherwise

pre_imputation_final$id<-factor(pre_imputation_final$id)


# Take out madeup variables

pre_imputation_final<-pre_imputation_final %>% select(-saf_madeup,-ags_madeup)

# Subset data ---------------------------------------------------------------

# Split into small subsets, parishes, otherwise the chainedEQ will fail 

list_subsets<-split(pre_imputation_final, pre_imputation_final$parish)

# Change variables to upper case

list_subsets<-lapply(list_subsets, function (x) dplyr::rename_with(x, toupper))

# Create four lists so that you don't have to run one massive imputation - in case something goes wrong in the middle. 

list_subsetsone<-list_subsets[1:200]
list_subsetstwo<-list_subsets[201:400]
list_subsetsthree<-list_subsets[401:600]
list_subsetsfour<-list_subsets[601:length(list_subsets)]

# The following bits could be done a lot more efficiently - will change!


list_subsetsone<-lapply(list_subsetsone, function(x){x$ID <- factor(x$ID,); x})

list_subsetsone<-lapply(list_subsetsone, function(x) {x<-as.data.frame(x); x})

list_subsetsone<-lapply(list_subsetsone, function(x) {rownames(x)<-1:nrow(x); x})

str(list_subsetsone[1])

rownames(list_subsetsone[[3]])



list_subsetstwo<-lapply(list_subsetstwo, function(x){x$ID <- factor(x$ID,); x})

list_subsetstwo<-lapply(list_subsetstwo, function(x) {x<-as.data.frame(x); x})

list_subsetstwo<-lapply(list_subsetstwo, function(x) {rownames(x)<-1:nrow(x); x})

str(list_subsetstwo[1])

rownames(list_subsetstwo[[3]])



list_subsetsthree<-lapply(list_subsetsthree, function(x){x$ID <- factor(x$ID,); x})

list_subsetsthree<-lapply(list_subsetsthree, function(x) {x<-as.data.frame(x); x})

list_subsetsthree<-lapply(list_subsetsthree, function(x) {rownames(x)<-1:nrow(x); x})

str(list_subsetsthree[1])

rownames(list_subsetsthree[[3]])



list_subsetsfour<-lapply(list_subsetsfour, function(x){x$ID <- factor(x$ID,); x})

list_subsetsfour<-lapply(list_subsetsfour, function(x) {x<-as.data.frame(x); x})

list_subsetsfour<-lapply(list_subsetsfour, function(x) {rownames(x)<-1:nrow(x); x})

str(list_subsetsfour[1])

rownames(list_subsetsfour[[3]])

rm(list_subsets, pre_imputation_reduced, pre_imputation_rolled_forward)

gc()


# Run imputation ----------------------------------------------------------

# This uses chainedEQ (method="pmm") which is a BIOSS wrapper for the mice package. 

# We run in four parts as the whole thing takes ~16 hours (20.09.23) 

# Run first imputation and save and remove outputs

old <- Sys.time()
list_jacs_imputed_chained_one<-pbapply::pblapply(list_subsetsone, function (x) impute(x, method="pmm", ts = "YR", noms = "ID", m = 20))
new <- Sys.time() - old 
print(new)

outputs_one<-lapply(
  list_jacs_imputed_chained_one, function (x) filter(x$results, YR == max(unique(YR), na.rm=TRUE)))

save(outputs_one, file = paste0(Code_directory, "/imputation_outputs_chainedEQ_200923_one.rda"))

rm(list_jacs_imputed_chained_one,outputs_one)
gc()

# Run second imputation and save and remove outputs

old2 <- Sys.time()
list_jacs_imputed_chained_two<-pbapply::pblapply(list_subsetstwo, function (x) impute(x, method="pmm", ts = "YR", noms = "ID", m = 20))
new2 <- Sys.time() - old 
print(new)

outputs_two<-lapply(
  list_jacs_imputed_chained_two, function (x) filter(x$results, YR == max(unique(YR), na.rm=TRUE)))

save(outputs_two, file = paste0(Code_directory, "/imputation_outputs_chainedEQ_200923_two.rda"))


rm(list_jacs_imputed_chained_two,outputs_two)
gc()

# Run third imputation and save and remove outputs

old3 <- Sys.time()
list_jacs_imputed_chained_three<-pbapply::pblapply(list_subsetsthree, function (x) impute(x, method="pmm", ts = "YR", noms = "ID", m = 20))
new3 <- Sys.time() - old 
print(new)

outputs_three<-lapply(
  list_jacs_imputed_chained_three, function (x) filter(x$results, YR == max(unique(YR), na.rm=TRUE)))

save(outputs_three, file = paste0(Code_directory, "/imputation_outputs_chainedEQ_200923_three.rda"))


rm(list_jacs_imputed_chained_three,outputs_three)
gc()


# Run fourth imputation and save and remove outputs

old4 <- Sys.time()
list_jacs_imputed_chained_four<-pbapply::pblapply(list_subsetsfour, function (x) impute(x, method="pmm", ts = "YR", noms = "ID", m = 20))
new4 <- Sys.time() - old 
print(new)

outputs_four<-lapply(
  list_jacs_imputed_chained_four, function (x) filter(x$results, YR == max(unique(YR), na.rm=TRUE)))

save(outputs_four, file = paste0(Code_directory, "/imputation_outputs_chainedEQ_200923_four.rda"))

rm(list_jacs_imputed_chained_four,outputs_four)
gc()


