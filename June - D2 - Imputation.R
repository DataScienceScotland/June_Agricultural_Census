# If you need to install imputeJAS, Move the imputejas-main.zip (Codeconversion_2023/BIOSS) into your TEMP folder and run the installation code below. 



# remotes::install_local("C:/temp/imputejas-main.zip",
#                        type="source", repos=NULL)


# Remake the pre_imputation dataset every week with new extract


library(RtoSQLServer)
library(dplyr)
library(janitor)
library(imputeJAS)


source("Functions/Functions.R")

Code_directory <- ("//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Codeconversion_2023/2023")

server <- "s0196a\\ADM"
database <- "RuralAndEnvironmentalScienceFarmingStatistics"
schema <- "juneagriculturalsurvey2023alpha"




# Load pre_imputation dataset

load(paste0(Code_directory, "/pre_imputation.rda"))


# Delete if yr is missing





# Rolling forward ---------------------------------------------------------


# Roll forward for SAF items which are rolled forward. If 2021 value is more than 10 years madeup, leave item blank.

check<-pre_imputation %>% 
  select(id, yr, item1, madeup, saf_madeup, ags_madeup)

rolled_forward_saf_items<-c("item1", "item2879", "item2827", "item2828", "item41", "item71")




rolled_forward_ags_items<-c()


# Multiple imputation -----------------------------------------------------



imputed_items<-c("item14","item24","item94","item139","item140","item141","item143","item144","item145","item2038","item2039",
                 "item2320","item2321","item2322","item2469","item2470","item2472","item2473","item2474","item2862","item2868",
                 "item27710","item27715","item27720","item27725","item27730","item27735","item27740","item27750",
                 "item27755","item27775","item27780") 



pre_imputation_reduced<-pre_imputation %>% 
  select(c("yr","id","parish","holding"),all_of(imputed_items))

check2023full<-pre_imputation_reduced %>% filter(yr=="2023")

# Add zeroes when 2023 is NA and previous years are zero

pre_imputation_reduced<-create_zeroes(pre_imputation_reduced)


pre_imputation_final <- keep_missing(pre_imputation_reduced)

none_missing<-none_missing(pre_imputation_reduced)

pre_imputation_final<-as.data.frame(pre_imputation_final)

pre_imputation_final$id<-droplevels(pre_imputation_final$id)

# Remove rows if no missing values in 2023

#pre_imputation<-keep_missing(pre_imputation)

# pre_imputation <- read_table_from_db(server=server, 
#                                      database=database, 
#                                      schema=schema, 
#                                      table_name="pre_imputation_reduced")


pre_imputation_final<-as.data.frame(pre_imputation_final)

str(pre_imputation_final)

check2023<-pre_imputation_final %>% filter(yr=="2023")

pre_imputation_final$id<-factor(pre_imputation_final$id)

#pre_imputation_reduced$id<-as.factor(pre_imputation_reduced$id)

# list_subsets<-list(
#   pre_imputation_reduced %>% 
#     filter(str_starts(id, '1')|str_starts(id, '2')|str_starts(id, '3')|str_starts(id, '4')) %>% 
#   select(c("yr","id"),3:10),
#   pre_imputation_reduced %>% 
#     filter(str_starts(id, '1')|str_starts(id, '2')|str_starts(id, '3')|str_starts(id, '4')) %>% 
#     select(c("yr","id"),11:18),
#   pre_imputation_reduced %>% 
#     filter(str_starts(id, '1')|str_starts(id, '2')|str_starts(id, '3')|str_starts(id, '4')) %>% 
#     select(c("yr","id"),19:26),
#   pre_imputation_reduced %>% 
#     filter(str_starts(id, '1')|str_starts(id, '2')|str_starts(id, '3')|str_starts(id, '4')) %>% 
#     select(c("yr","id"),27:34),
#   pre_imputation_reduced %>% 
#     filter(str_starts(id, '5')|str_starts(id, '6')|str_starts(id, '7')|str_starts(id, '8')|str_starts(id, '9')) %>% 
#     select(c("yr","id"),3:10),
#   pre_imputation_reduced %>% 
#     filter(str_starts(id, '5')|str_starts(id, '6')|str_starts(id, '7')|str_starts(id, '8')|str_starts(id, '9')) %>% 
#     select(c("yr","id"),11:18), 
#   pre_imputation_reduced %>% 
#     filter(str_starts(id, '5')|str_starts(id, '6')|str_starts(id, '7')|str_starts(id, '8')|str_starts(id, '9')) %>% 
#     select(c("yr","id"),19:26),
#   pre_imputation_reduced %>% 
#     filter(str_starts(id, '5')|str_starts(id, '6')|str_starts(id, '7')|str_starts(id, '8')|str_starts(id, '9')) %>% 
#     select(c("yr","id"),27:34)
# )

list_subsets<-split(pre_imputation_final, pre_imputation_final$parish)


#list_subsets<-split(pre_imputation_reduced, substr(pre_imputation_reduced$id,1,1))
# check rownames of each df in list_subsets

list_subsets<-lapply(list_subsets, function (x) dplyr::rename_with(x, toupper))

str(list_subsets[1])

list_subsets<-lapply(list_subsets, function(x){x$ID <- factor(x$ID,); x})

str(list_subsets[1])



list_subsets<-lapply(list_subsets, function(x) { x["PARISH"] <- NULL; x })
list_subsets<-lapply(list_subsets, function(x) { x["HOLDING"] <- NULL; x })

list_subsets<-lapply(list_subsets, function(x){x$ID <- droplevels(x$ID,); x})

list_subsets<-lapply(list_subsets, function(x) {rownames(x)<-1:nrow(x); x})



list_subsetstest<-split(pre_imputation_final, pre_imputation_final$id)


list_subsetstest<-lapply(list_subsetstest, function (x) dplyr::rename_with(x, toupper))

str(list_subsetstest[1])

list_subsetstest<-lapply(list_subsetstest, function(x){x$ID <- factor(x$ID,); x})

str(list_subsetstest[1])



list_subsetstest<-lapply(list_subsetstest, function(x) { x["PARISH"] <- NULL; x })
list_subsetstest<-lapply(list_subsetstest, function(x) { x["HOLDING"] <- NULL; x })

list_subsetstest<-lapply(list_subsetstest, function(x){x$ID <- droplevels(x$ID,); x})

list_subsetstest<-lapply(list_subsetstest, function(x) {rownames(x)<-1:nrow(x); x})


list_subsetsone<-as.data.frame(list_subsets[[1]])
check<-pre_imputation_reduced %>% 
  filter(stringr::str_starts(id, '7'))
check<-dplyr::rename_with(check, toupper)
outputcheck<-bootstrapEM(test, ts = "YR", cs = "ID", m = 20)

test<-pre_imputation_final %>% 
  select(c(1:33)) %>% 
  filter(parish==2) %>% 
  rename_with(toupper)
  
old <- Sys.time()
list_jacs_imputed<-lapply(list_subsetsone, function (x) bootstrapEM(x, ts = "YR", cs = "ID", m = 20))
new <- Sys.time() - old 
print(new)

outputs<-filter(list_jacs_imputed$results, YR == max(unique(YR), na.rm=TRUE))

means <- outputs %>%
  group_by(ID) %>%
  summarise(across(where(is.numeric), list(mean=mean, sd=sd)))


item139<-pre_imputation_reduced %>% 
  select(yr,id,item139) %>% 
  filter(id=="28_218"|id=="223_98")


output139<-outputs %>% 
  select(YR,ID,IMP,ITEM139) %>% 
  filter(ID=="28_218"|ID=="223_98")
old <- Sys.time()
list_jacs_imputed<-lapply(list_subsets, function (x) bootstrapEM(x, ts = "YR", cs = "ID", m = 20))
new <- Sys.time() - old 
print(new)

negativeoutputs<-list_jacs_imputed$results

negativeoutputs<-negativeoutputs %>% 
  filter(across(where(is.numeric), < 0))




outputs<-lapply(
  list_jacs_imputed, function (x) filter(x$results, YR == max(unique(YR), na.rm=TRUE)))
save
rm(list_jacs_imputed)

gc()

# Combine dfs into one


a<-outputs[1:4] %>% reduce(full_join, by=c('ID','IMP','YR'))

b<-outputs[5:8] %>% reduce(full_join, by=c('ID','IMP','YR'))

output_bootstrapEM<-rbind(a,b)

save(outputs, file = paste0(Code_directory, "/imputation_outputs_bootstrapEM_100823.rda"))

# get means and confidence intervals

load(paste0(Code_directory, "/imputation_outputs_bootstrapEM_100823.rda"))

str(output_bootstrapEM)

means <- output_bootstrapEM %>%
  group_by(ID) %>%
  summarise(across(where(is.numeric), list(mean=mean, sd=sd)))

pre_imputation <- read_table_from_db(server=server,
                                     database=database,
                                     schema=schema,
                                     table_name="pre_imputation_currentyear")




# Alternative functions  --------------------------------------------------



# Try chainedEQ. # Try subsetting by parish. Need to keep parish and holding in pre-imputation script. Use the same for bootstrapEM?

data(JACdat_eg)
eglist<-split(JACdat_eg, JACdat_eg$ID)

list_subsetschained<-dplyr::rename_with(list_subsetschained, toupper)
str(list_subsetschained)

list_subsetschained$ID<-factor(list_subsetschained$ID)

str(list_subsetschained)

list_subsetsone<-list_subsets[1:250]
list_subsetstwo<-list_subsets[251:500]
list_subsetsthree<-list_subsets[501:750]
list_subsetsfour<-list_subsetsone[751:length(list_subsets)]

rm(list_subsets)

old <- Sys.time()
list_jacs_imputed_chained<-pbapply::pblapply(list_subsets, function (x) chainedEQ(x, method="pmm", ts = "YR", noms = "ID", m = 20))
new <- Sys.time() - old 
print(new)

outputs<-lapply(
  jacs_imputed_chained, function (x) filter(x$results, YR == max(unique(YR), na.rm=TRUE)))


for(i in 1:length(list_subsets)) {        # Loop from 1 to length of list
  print(i)
  list_outputs[[i]]<-chainedEQ(list_subsets[[i]], ts = "YR", noms = "ID", m = 20)  
}

check<-chainedEQ(JACdat_eg, ts = "YR", noms = "ID", m = 5)
df1<-data.frame(list_subsets[1])
df1<-df1 %>% 
  select(1:3) %>% 
  filter(str_starts(ID, '1')& YR>2021)

old <- Sys.time()
jacs_imputed_chained<-chainedEQ(df1, method = "pmm", ts = "YR", noms = "ID", m = 5)
new <- Sys.time() - old 
print(new)

pre_imp<-dplyr::rename_with(pre_imputation_reduced, toupper)

JAC_286 <- pre_imp %>% 
 filter(str_starts(ID, '286'))



dim(JAC_286)

rownames(JAC_286) <- 1:2891
names(JAC_286)

JAC_286[sample(1:2313,200), 3:34] <- NA 

JAC_286$ID<-factor(JAC_286$ID)


old <- Sys.time()
jacs_imputed_chained<-lapply(list_subsets[1], function (x) chainedEQ (x,ts = "YR", noms = "ID", m = 10))
new <- Sys.time() - old 
print(new)




rm(list_jacs_imputed)

gc()

# Try with pmm

# Try forest. 40 mins.
df1<-data.frame(list_subsets[1])
df1<-df1 %>% 
  select(1:10) %>% 
  filter(str_starts(ID, '1'))

old <- Sys.time()
forest<-forest(df1, ts = "YR", noms = "ID", m = 5)
new <- Sys.time() - old 
print(new)



