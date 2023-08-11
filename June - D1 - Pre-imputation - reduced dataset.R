
library(imputeJAS)
library(RtoSQLServer)
library(dplyr)
library(janitor)
library(stringr)




Code_directory <- ("//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Codeconversion_2023/2023")

server <- "s0196a\\ADM"
database <- "RuralAndEnvironmentalScienceFarmingStatistics"
schema <- "juneagriculturalsurvey2023alpha"


previous_years <- read_table_from_db(server=server, 
                                     database=database, 
                                     schema=schema, 
                                     table_name="jac_previous_data_ten_years")



previous_years[previous_years==""] <- NA


previous_years <- previous_years %>% 
  rename(
    item27710=barley_27710,
    item27715=oats_27715,
    item27720=osr_27720,
    item27725=stockfeed_27725,
    item27730=veginopen_27730,
    item27735=fruitinopen_27735,
    item27740=nursery_27740,
    item27750=opensoil_27750,
    item27755=solidfloor_27755,
    item27775=horses_27775,
    item27780=goats_27780
    
  )


# Load combined_data_2023


data_2023 <- read_table_from_db(server=server, 
                                database=database, 
                                schema=schema, 
                                table_name="combined_data_2023")


data_2023<-data_2023 %>% 
  mutate(item12=ifelse(item12==0 & item11>0, rps_totarea_june, item12))



# Check NAs are zero

# Load population frame


addressfileorig<-read_table_from_db(server=server, 
                                    database=database, 
                                    schema=schema, 
                                    table_name="address_occid_01jun_2023")

holding_classification<-c(1,2,4,5,6,7,13)

population<-addressfileorig %>% 
  filter(holding_classification_id %in% holding_classification)

population<-population %>% 
  select(parish,holding,tot_area,tot_own_area)

# Combine population with 2023 data 

pre_imputation_2023<-left_join(population, data_2023, by=c("parish", "holding"))



pre_imputation_2023<-pre_imputation_2023 %>% 
  mutate(
    item11=ifelse(is.na(item12), tot_own_area, item11),
    item20026=ifelse(is.na(item12), (tot_area-tot_own_area), item20026),
    item12=ifelse(is.na(item12), tot_area,item12)
  ) %>% 
  mutate(
    survdata=ifelse(is.na(combined_data_2023ID), "none", survdata),
    land_data=ifelse(is.na(combined_data_2023ID), "none", land_data),
    other_data=ifelse(is.na(combined_data_2023ID), "no", other_data),
    imptype=ifelse(is.na(combined_data_2023ID), "full", imptype),
    survtype=ifelse(is.na(combined_data_2023ID), "none", survtype),
    
  )


not_in_population<-anti_join(data_2023,population, by=c("parish", "holding"))


# Save this pre_imputation dataset to datashare and ADM



# Combine 2023 with previous years and prep for imputation

pre_imputation_2023$yr<-"2023"

pre_imputation_2023$yr<-as.numeric(pre_imputation_2023$yr)


write_dataframe_to_db(server=server,
                      database=database,
                      schema=schema,
                      table_name="pre_imputation_currentyear",
                      dataframe=pre_imputation_2023,
                      append_to_existing = FALSE,
                      batch_size=10000,
                      versioned_table=FALSE)


pre_imputation_2023 <- read_table_from_db(server=server, 
                                database=database, 
                                schema=schema, 
                                table_name="pre_imputation_currentyear")

pre_imputation_2023<-pre_imputation_2023%>% 
  filter(!imptype=="none")


write_dataframe_to_db(server=server,
                      database=database,
                      schema=schema,
                      table_name="pre_imputation_2023",
                      dataframe=pre_imputation_2023,
                      append_to_existing = FALSE,
                      batch_size=10000,
                      versioned_table=FALSE)


imputed_items<-c("item14","item24","item94","item139","item140","item141","item143","item144","item145","item2038","item2039",
                 "item2320","item2321","item2322","item2469","item2470","item2472","item2473","item2474","item2862","item2868",
                 "item27710","item27715","item27720","item27725","item27730","item27735","item27740","item27750",
                 "item27755","item27775","item27780") 

pre_imputation_2023_reduced$id<-paste0(pre_imputation_2023_reduced$parish,"_",pre_imputation_2023_reduced$holding)

pre_imputation_2023_reduced<-pre_imputation_2023_reduced %>% 
  select(c("yr","id"),all_of(imputed_items))


previous_years$id<-paste0(previous_years$parish,"_",previous_years$holding)

previous_years<-previous_years %>% 
  select(c("yr","id"),all_of(imputed_items))

previous_years<-subset(previous_years, id %in% pre_imputation_2023_reduced$id)

pre_imputation_reduced<-rbind(pre_imputation_2023_reduced,previous_years)



pre_imputation_reduced$id<-as.factor(pre_imputation_reduced$id)


pre_imputation_reduced<-pre_imputation_reduced %>% 
  filter_all(any_vars(!is.na(.)))

pre_imputation_reduced$id<-as.factor(pre_imputation_reduced$id)


pre_imputation_reduced<-pre_imputation_reduced %>% 
  filter(!is.na(yr))

pre_imputation_reduced[pre_imputation_reduced==""] <- NA

write_dataframe_to_db(server=server,
                      database=database,
                      schema=schema,
                      table_name="preimputation_reduced",
                      dataframe=pre_imputation_reduced,
                      append_to_existing = FALSE,
                      batch_size=10000,
                      versioned_table=FALSE)

save(pre_imputation_reduced, file = paste0(Code_directory, "/pre_imputation_reduced_holdings.rda"))
