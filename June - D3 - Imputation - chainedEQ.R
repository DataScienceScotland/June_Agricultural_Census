# This script takes in the pre-imputation dataset rolled forward dataset and performs multiple imputation for imputed items. 
# There are two D3 scripts. This one is for the function chaineEQ. The other is for the less time-consuming function bootstrapEM. Only one will be run in the final cut.
# Created by Lucy Nevard June 2023. 
# Modified by Lucy Nevard 04.10.23


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


source("Functions/Functions.R")
source("item_numbers.R")

# Import ------------------------------------------------------------------


# Load pre_imputation dataset

load(paste0(Code_directory, "/pre_imputation_rolled_forward.rda"))


# pre_imputation_rolled_forward is a large dataset and is time-consuming to read in from ADM - read in from the datashare instead if needed. 

# pre_imputation_rolled_forward<- read_table_from_db(server=server, 
#                                      database=database, 
#                                      schema=schema, 
#                                      table_name="pre_imputation_rolled_forward")



# Multiple imputation -----------------------------------------------------


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

# Remove unwanted variables

list_subsets<-lapply(list_subsets, function(x) x[(names(x) %in% c("yr", "id", imputed_items))])


# Change variables to upper case

list_subsets<-lapply(list_subsets, function (x) dplyr::rename_with(x, toupper))



# Create four lists so that you don't have to run one massive imputation - in case something goes wrong in the middle. 

list_subsetsone<-list_subsets[1:300]
list_subsetstwo<-list_subsets[301:500]
list_subsetsthree<-list_subsets[501:700]
list_subsetsfour<-list_subsets[701:length(list_subsets)]

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




# Pig imputation ----------------------------------------------------------

# Keep only when SAF not filled in, imptype=full

pre_imputation_pig<-pre_imputation_rolled_forward %>% 
  group_by(id) %>% 
  filter(any(imptype=="full"&yr==2023))


# Keep only imputed items in the dataset
pig_imputed_items<-c("item146", "item147","item148", "item149","item150", "item151", "item27760","item27770")


pre_imputation_pig<-pre_imputation_pig %>% 
  select(c("yr","id","parish","holding", "ags_madeup", "saf_madeup"),all_of(pig_imputed_items))

pre_imputation_pig<-as.data.frame(pre_imputation_pig)

# Add zeroes for items when 2023 is NA and all historic data are zero

pre_imputation_pig_zeroes<-create_zeroes(pre_imputation_pig)

# Add zeroes when ags_madeup >=10 and saf_madeup>=10 

madeupovertenpig<-create_df_madeup(pre_imputation_pig_zeroes)
madeupovertenpig<-create_zeroes_madeup(madeupovertenpig)

# Recreate dataset with zeroes

pre_imputation_pig_zeroes<-rows_update(pre_imputation_pig_zeroes, madeupovertenpig, by=c("id", "yr"))

# Save this so that the imputed items can be added back to it in D4

save(pre_imputation_pig_zeroes, file = paste0(Code_directory, "/pre_imputation_pig_zeroes.rda"))

# Keep holdings only when 2023 has NAs. This should reduce the size of the dataset significantly, .e.g end up with ~310000 rows (as of 26/09/23. this will be fewer as we receive more census responses)

pre_imputation_pig_final <- keep_missing(pre_imputation_pig_zeroes)

pre_imputation_pig_final<-as.data.frame(pre_imputation_pig_final)

pre_imputation_pig_final<-pre_imputation_pig_final %>% 
  ungroup()

str(pre_imputation_pig_final)

pre_imputation_pig_final$id<-factor(pre_imputation_pig_final$id)

pre_imputation_pig_final<-pre_imputation_pig_final %>% 
  select(c("yr","id"),all_of(pig_imputed_items))


pig_subsets<-list(
  pre_imputation_pig_final %>%
    filter(str_starts(id, '1')),
  pre_imputation_pig_final %>%
    filter(str_starts(id, '2')),
  pre_imputation_pig_final %>%
    filter(str_starts(id, '3')),
  pre_imputation_pig_final %>%
    filter(str_starts(id, '4')),
  pre_imputation_pig_final %>%
    filter(str_starts(id, '5')),
  pre_imputation_pig_final %>%
    filter(str_starts(id, '6')),
  pre_imputation_pig_final %>%
    filter(str_starts(id, '7')),
  pre_imputation_pig_final %>%
    filter(str_starts(id, '8')),
  pre_imputation_pig_final %>%
    filter(str_starts(id, '9'))
)
  

# Change variables to upper case

pig_subsets<-lapply(pig_subsets, function (x) dplyr::rename_with(x, toupper))


# Keep only levels of factor which exist in each dataframe

pig_subsets<-lapply(pig_subsets, function(x){x$ID <- factor(x$ID,); x})

str(pig_subsets[4])

# Change row numbers to 1->x for each dataframe

pig_subsets<-lapply(pig_subsets, function(x) {x<-as.data.frame(x); x})

pig_subsets<-lapply(pig_subsets, function(x) {rownames(x)<-1:nrow(x); x})

str(pig_subsets[3])

rownames(pig_subsets[[3]])







rm(list_subsets, pre_imputation_reduced, pre_imputation_rolled_forward)

gc()


# Run imputation ----------------------------------------------------------

# This uses chainedEQ (method="pmm") which is a BIOSS wrapper for the mice package. 

# We run in four parts as the whole thing takes ~16 hours (27.09.23) 

# Run first imputation and save and remove outputs

old <- Sys.time()
list_jacs_imputed_chained_one<-pbapply::pblapply(list_subsetsone, function (x) impute(x, method="pmm", ts = "YR", noms = "ID", m = 20))
new <- Sys.time() - old 
print(new)

outputs_one<-lapply(
  list_jacs_imputed_chained_one, function (x) filter(x$results, YR == max(unique(YR), na.rm=TRUE)))

save(outputs_one, file = paste0(Code_directory, "/imputation_outputs_chainedEQ_041023_one.rda"))

rm(list_jacs_imputed_chained_one,outputs_one)
gc()

# Run second imputation and save and remove outputs

old2 <- Sys.time()
list_jacs_imputed_chained_two<-pbapply::pblapply(list_subsetstwo, function (x) impute(x, method="pmm", ts = "YR", noms = "ID", m = 20))
new2 <- Sys.time() - old2
print(new)

outputs_two<-lapply(
  list_jacs_imputed_chained_two, function (x) filter(x$results, YR == max(unique(YR), na.rm=TRUE)))

save(outputs_two, file = paste0(Code_directory, "/imputation_outputs_chainedEQ_041023_two.rda"))


rm(list_jacs_imputed_chained_two,outputs_two)
gc()

# Run third imputation and save and remove outputs

old3 <- Sys.time()
list_jacs_imputed_chained_three<-pbapply::pblapply(list_subsetsthree, function (x) impute(x, method="pmm", ts = "YR", noms = "ID", m = 20))
new3 <- Sys.time() - old3 
print(new)

outputs_three<-lapply(
  list_jacs_imputed_chained_three, function (x) filter(x$results, YR == max(unique(YR), na.rm=TRUE)))

save(outputs_three, file = paste0(Code_directory, "/imputation_outputs_chainedEQ_041023_three.rda"))


rm(list_jacs_imputed_chained_three,outputs_three)
gc()


# Run fourth imputation and save and remove outputs

old4 <- Sys.time()
list_jacs_imputed_chained_four<-pbapply::pblapply(list_subsetsfour, function (x) impute(x, method="pmm", ts = "YR", noms = "ID", m = 20))
new4 <- Sys.time() - old4 
print(new)

outputs_four<-lapply(
  list_jacs_imputed_chained_four, function (x) filter(x$results, YR == max(unique(YR), na.rm=TRUE)))

save(outputs_four, file = paste0(Code_directory, "/imputation_outputs_chainedEQ_041023_four.rda"))

rm(list_jacs_imputed_chained_four,outputs_four)
gc()



# Run pig imputation

old <- Sys.time()
pigs_jacs_imputed_chained<-pbapply::pblapply(pig_subsets, function (x) impute(x, method="pmm", ts = "YR", noms = "ID", m = 20))
new <- Sys.time() - old 
print(new)


outputspigs<-lapply(
  pigs_jacs_imputed_chained,function (x) filter(x$results, YR == max(unique(YR), na.rm=TRUE)))

save(outputspigs, file = paste0(Code_directory, "/jac_pigs_chainedEQ_041023.rda"))
