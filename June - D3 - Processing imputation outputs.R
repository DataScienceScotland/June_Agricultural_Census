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

load(paste0(Code_directory, "/imputation_outputs_bootstrapEM_100823.rda"))
 
 
# get means and confidence intervals

str(output_bootstrapEM)

means <- output_bootstrapEM %>%
  group_by(ID) %>%
  summarise(across(where(is.numeric), list(mean=mean, sd=sd)))

# Keep means which are negative to check

means<-clean_names(means)

negativemeans<-means %>% 
  select(id,item14_mean) %>% 
  filter(item14_mean<0)


pre_14<-pre_imputation_reduced %>% 
  select(id, yr, item14)

check<-left_join(negativemeans, pre_14, by="id")


check2<-check %>% 
  filter(item14>0)