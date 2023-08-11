# If you need to install imputeJAS, Move the imputejas-main.zip (Codeconversion_2023/BIOSS) into your TEMP folder and run the installation code below. 



# remotes::install_local("C:/temp/imputejas-main.zip",
#                        type="source", repos=NULL)




library(imputeJAS)
library(RtoSQLServer)
library(dplyr)
library(janitor)
library(stringr)



Code_directory <- ("//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Codeconversion_2023/2023")

server <- "s0196a\\ADM"
database <- "RuralAndEnvironmentalScienceFarmingStatistics"
schema <- "juneagriculturalsurvey2023alpha"




# Load pre_imputation dataset

load(paste0(Code_directory, "/pre_imputation_reduced_holdings.rda"))

# pre_imputation <- read_table_from_db(server=server, 
#                                      database=database, 
#                                      schema=schema, 
#                                      table_name="pre_imputation_reduced")


str(pre_imputation_reduced)

check2023<-pre_imputation_reduced %>% filter(yr=="2023")


#pre_imputation_reduced$id<-as.factor(pre_imputation_reduced$id)

list_subsets<-list(
  pre_imputation_reduced %>% 
    filter(str_starts(id, '1')|str_starts(id, '2')|str_starts(id, '3')|str_starts(id, '4')) %>% 
  select(c("yr","id"),3:10),
  pre_imputation_reduced %>% 
    filter(str_starts(id, '1')|str_starts(id, '2')|str_starts(id, '3')|str_starts(id, '4')) %>% 
    select(c("yr","id"),11:18),
  pre_imputation_reduced %>% 
    filter(str_starts(id, '1')|str_starts(id, '2')|str_starts(id, '3')|str_starts(id, '4')) %>% 
    select(c("yr","id"),19:26),
  pre_imputation_reduced %>% 
    filter(str_starts(id, '1')|str_starts(id, '2')|str_starts(id, '3')|str_starts(id, '4')) %>% 
    select(c("yr","id"),27:34),
  pre_imputation_reduced %>% 
    filter(str_starts(id, '5')|str_starts(id, '6')|str_starts(id, '7')|str_starts(id, '8')|str_starts(id, '9')) %>% 
    select(c("yr","id"),3:10),
  pre_imputation_reduced %>% 
    filter(str_starts(id, '5')|str_starts(id, '6')|str_starts(id, '7')|str_starts(id, '8')|str_starts(id, '9')) %>% 
    select(c("yr","id"),11:18), 
  pre_imputation_reduced %>% 
    filter(str_starts(id, '5')|str_starts(id, '6')|str_starts(id, '7')|str_starts(id, '8')|str_starts(id, '9')) %>% 
    select(c("yr","id"),19:26),
  pre_imputation_reduced %>% 
    filter(str_starts(id, '5')|str_starts(id, '6')|str_starts(id, '7')|str_starts(id, '8')|str_starts(id, '9')) %>% 
    select(c("yr","id"),27:34)
)


list_subsets<-lapply(list_subsets, function (x) dplyr::rename_with(x, toupper))



old <- Sys.time()
list_jacs_imputed<-lapply(list_subsets, function (x) bootstrapEM(x, ts = "YR", cs = "ID", m = 10))
new <- Sys.time() - old 
print(new)


outputs<-lapply(
  list_jacs_imputed, function (x) filter(x$results, YR == max(unique(YR), na.rm=TRUE)))



rm(list_jacs_imputed)

gc()

# Combine dfs into one


a<-outputs[1:4] %>% reduce(full_join, by=c('ID','IMP','YR'))

b<-outputs[5:8] %>% reduce(full_join, by=c('ID','IMP','YR'))

output_bootstrapEM<-rbind(a,b)

save(output_bootstrapEM, file = paste0(Code_directory, "/imputation_outputs_bootstrapEM_100823.rda"))

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



# Try chainedEQ. ! cannot allocate vector of size 13.4 Gb

data(JACdat_eg)


check<-chainedEQ(JACdat_eg, ts = "YR", noms = "ID", m = 5)
df1<-data.frame(list_subsets[1])
df1<-df1 %>% 
  select(1:3) %>% 
  filter(str_starts(ID, '1')& YR>2021)

old <- Sys.time()
jacs_imputed_chained<-chainedEQ(df1, method = "pmm", ts = "YR", noms = "ID", m = 5)
new <- Sys.time() - old 
print(new)


# Try forest. 40 mins.
df1<-data.frame(list_subsets[1])
df1<-df1 %>% 
  select(1:10) %>% 
  filter(str_starts(ID, '1'))

old <- Sys.time()
forest<-forest(df1, ts = "YR", noms = "ID", m = 5)
new <- Sys.time() - old 
print(new)