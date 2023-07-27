# If you need to install imputeJAS, Move the imputejas-main.zip (Codeconversion_2023/BIOSS) into your TEMP folder and run the installation code below. 



# remotes::install_local("C:/temp/imputejas-main.zip",
#                        type="source", repos=NULL)




library(imputeJAS)
library(RtoSQLServer)
library(dplyr)
library(janitor)

source("Scripts/Item categories.R")



Code_directory <- ("//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Codeconversion_2023/2023")

server <- "s0196a\\ADM"
database <- "RuralAndEnvironmentalScienceFarmingStatistics"
schema <- "juneagriculturalsurvey2023alpha"

# Load real_data for 3 previous years - 2019, 2020 and 2021



previous_years <- read_table_from_db(server=server, 
                                   database=database, 
                                   schema=schema, 
                                   table_name="jac_previous_data_19_20_21")


# Load combined_data_2023


data_2023 <- read_table_from_db(server=server, 
                                database=database, 
                                schema=schema, 
                                table_name="combined_data_2023")


# Create pre-imputation 2023 dataset by compining with sample
# Check NAs are zero
# Add sample



# Combine 2023 with previous years and prep for imputation

data_2023$yr<-"2023"

data_2023$yr<-as.numeric(data_2023$yr)

real_data<-dplyr::bind_rows(previous_years,data_2023)

real_data$id<-paste0(real_data$parish,"_",real_data$holding)



# Create a list of subsets of the data 


item_subsets<-list(
  real_data %>% select(c("yr", "id", all_of(all_fruit))),
  real_data %>% select(c("yr", "id", all_of(all_nursery))),
  real_data %>% select(c("yr", "id", all_of(all_pig)))
)


item_subsets<-lapply(item_subsets, function (x) dplyr::rename_with(x, toupper))

all_fruit23<-item_subsets[[1]]

# all_fruit23<-all_fruit23 %>% 
#   filter(!(YR==2019))

imputed_data <-  bootstrapEM(all_fruit23, ts = "YR", cs = "ID", m = 10)


old <- Sys.time()
list_imputed_data <-  lapply(item_subsets,
                             function(x)
                               bootstrapEM(x,  ts = "YR", cs = "ID", m = 10))
new <- Sys.time() - old # calculate difference
print(new)



# # Run through one of the less computationally intensive imputation techniques
# ?bootstrapEM # Look at documentation (which will be improved)
# 
# # using example data
# 
# #imputed_data <- bootstrapEM(JACdat_eg, ts = "YR", cs = "ID", m = 3)
# 
# #imputed_data<-impute(JACdat_eg, method = "mean", ts = "YR", noms = "ID", m = 5)
# 
# # using real data
# 
# old <- Sys.time()
# imputed_realdata <- bootstrapEM(real_data, ts = "YR", cs = "ID", m = 1)
# new <- Sys.time() - old # calculate difference
# print(new) 
# 
# save(imputed_realdata, file =  "C:/Users/u455049/OneDrive - SCOTS Connect/June/imputed_realdata.rda")
# 
# imputed_data<-impute(real_data, method = "bootstrapEM", ts = "YR", noms = "ID", m = 3)

# Output dataset for your downstream processing
output_data <- imputed_data$results %>% 
  filter(YR == max(unique(imputed_data$results$YR)))
