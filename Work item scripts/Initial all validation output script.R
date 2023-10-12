#Validation outputs for Ops to investigate


library(tidyverse)
library(RtoSQLServer)
library(writexl)
library(readxl)

# Clear environment prior
rm(list = ls())



# Before import -----------------------------------------------------------

# Datashare file path for import and export
output_path <- "//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Work_items_Ops/"
#sas_agscens_path <- "//s0177a/sasdata1/ags/census/agscens/"

#load(paste(output_path, "2023/Ags_A_2023.rda", sep = ""))

# ADM schema for export
server <- "s0196a\\ADM"
database <- "RuralAndEnvironmentalScienceFarmingStatistics"
schema <- "juneagriculturalsurvey2023alpha"

# main validation descriptions ------------------------------------------
err1_desc <- "err1_Total area does not equal total area owned plus total area rented (by 3 or more hectares)"
err10_desc <- "err10_Sum of itemised flowers bulbs and nursery stock in the open not equal to summary total"
err11_desc <- "err11_Total glasshouse open soil area does not equal the sum of the breakdown of areas OR Total glasshouse solid floor area does not equal the sum of the breakdown of areas"
err12_desc <- "err12_Glasshouse totals non zero but sum of individual items less than or equal to zero"
err13_desc <- "err13_Total number of pigs does not equal the sum of the breakdown values"
err14_desc <- "err14_Pigs not given as whole numbers"
err15_desc <- "err15_Total number of sheep does not equal the sum of the breakdown values"
err16_desc <- "err16_Sheep not given as whole numbers"
err17_desc <- "err17_Total number of poultry does not equal the sum of the breakdown values"
err18_desc <- "err18_Poultry not given as whole numbers"
err19_desc <- "err19_Egg producing fowl = 0, cocks greater than 9"
err2_desc <- "err2_Total seasonal rents is equal to area rented"
err20_desc <- "err20_Not all deer given as whole numbers"
err21_desc <- "err21_Not all camelids given as whole numbers"
err22_desc <- "err22_Not all beehives given as whole numbers"
err23_desc <- "err23_Not all other livestock given as whole numbers"
err24_desc <- "err24_Number of other livestock entered but type not specified"
err25_desc <- "err25_Not all horses given as whole numbers"
err26_desc <- "err26_Not all goats given as whole numbers"
err27_desc <- "err27_Not all horses given as whole numbers"
err28_desc <- "err28_Not all goats given as whole numbers"
err29_desc <- "err29_Other legal return box is ticked (labour details provided on another form) and Occupier 1 or occupier 2 work hours provided"
err3_desc <- "err3_Seasonal let items are not equal to seasonal lets total"
err30_desc <- "err30_Occupier 1: more than one full time / part time option selected"
err31_desc <- "err31_Occupier 2: more than one full time / part time option selected"
err32_desc <- "err32_Sum of other labour numbers does not equal Total"
err33_desc <- "err33_Not all labour given as whole numbers"
err34_desc <- "err34_Owned croft area is larger than area owned"
err35_desc <- "err35_Owned croft area >0 and  is in non-crofting parish"
err36_desc <- "err36_Rented croft area >0 and  is in non-crofting parish"
err37_desc <- "err37_Total area less than margin of error - no area entered but area owned/rented non-zero"
err38_desc <- "err38_Total area less than margin of error - no total area, no area owned and no area rented entered"
err39_desc <- "err39_Migrant labour check: Soft fruit labour is too low-check whether missed other labour e.g. migrants "
err4_desc <- "err4_Area of other crop entered but crop not specified (Area > 100ha) "
err40_desc <- "err40_Migrant labour check: Soft fruit labour is too high- check for punch errors or misunderstanding of requirement of people/days in labour questions"
err41_desc <- "err41_Migrant labour check: Soft fruit labour is too low- check whether missed other labour e.g. migrants"
err42_desc <- "err42_Migrant labour check: Soft fruit labour is too high- check for punch errors or misunderstanding of requirement of people/days in labour questions"
err43_desc <- "err43_Land seasonally rented out greater than total area"
err44_desc <- "err44_Other crop specified but area not entered"
err45_desc <- "err45_Total land use area does not equal the sum of the outdoor areas and glasshouses (3 ha margin)"
err46_desc <- "err46_Total at end of land use section does not equal total area in section 1 (3 ha margin)"
err47_desc <- "err47_Other livestock type specified but number not entered"
err48_desc <- "err48_Occupier gender - both male and female selected"
err49_desc <- "err49_Occupier 1 gender selected but none of the full time / part time options"
err5_desc <- "err5_Total area of crops and grassland, rough grazing, woodland, and other land differs from total land by more than 3ha."
err50_desc <- "err50_Occupier 1 gender selected but none of the full time / part time options"
err51_desc <- "err51_Year of birth for Occupier 1 (and Occupier 2 if applicable) outside range (i.e. 1923 - 2008)"
err52_desc <- "err52_Labour details NOT provided on another form this year & no Occupier 1 work hours provided"
err53_desc <- "err53_Legal and financial responsbility not indicated for Occupier 2 (when any Occupier 2 details have been provided for Occupier 2)"
err54_desc <- "err54_Legal and financial responsbility indicated for Occupier 2 but no working time answered"
err55_desc <- "err55_Occupier 2 details selected but not Occupier 1 details"
err56_desc <- "err56_Total area rented not equal to the current RP&S value (by 3 or more hectares)"
err57_desc <- "err57_Total area rented not equal to the current RP&S value (by 30 or more hectares)"
err58_desc <- "err58_Total area owned not equal to the current RP&S value (by 3 or more hectares)"
err59_desc <- "err59_Total area owned not equal to the current RP&S value (by 30 or more hectares)"
err6_desc <- "err6_Sum of itemised veg for human consumption not equal to all veg (SAF)"
err7_desc <- "err7_All veg from SAF not equal to all veg (census form)"
err8_desc <- "err8_Sum of itemised soft fruit not equal to all soft fruit (SAF)"
err9_desc <- "err9_All soft fruit from SAF not equal to all soft fruit (census form)"
err60_desc <- "err60_Legal responsbility box is not ticked (item2727) and no legal responsibility details (item2980) are given"
err61_desc <- "err61_Sum of total area rented and total area owned not equal to sum of RP&S values (by 30 or more hectares)"

#main validation list
main_validation_list <- paste0("err", 1:61)

# Context error descriptions ------------------------------------------
perr1_desc <- "perr1_Old grass increased to more than all grass last year"
perr2_desc <- "perr2_Large change in pigs"
perr3_desc <- "perr3_Suspicious change in poultry numbers"
perr4_desc <- "perr4_Large change in total cattle"
perr5_desc <- "perr5_Introduction of dairy to beef holding"
perr6_desc <- "perr6_Introduction of beef to dairy holding"
perr7_desc <- "perr7_Large change in total sheep numbers"
perr9_desc <- "perr9_Large change in soft fruit area"
perr10_desc <- "perr10_Large change in glasshouse area"
perr11_desc <- "perr11_Introduction of male cattle to holding with only female cattle (excluding bulls for service)"
perr12_desc <- "perr12_Introduction of female cattle not for breeding to holding with only male cattle"
perr13_desc <- "perr13_Switch from ware to seed potatoes"
perr14_desc <- "perr14_Large shift in deer numbers or movement from zero or to zero"
perr15_desc <- "perr15_Large change in total land area"
perr16_desc <- "perr16_Large change in rough grazing area"
perr17_desc <- "perr17_Large change in migrant labour"

#context error list
context_error_list <- paste0("perr", c(1:7, 9:17)) 




# Module error descriptions ------------------------------------------

merr1_desc <- "merr1_Total percentage of the manure/slurry spread not equal to 100 percent"
merr2_desc <- "merr2_Total percentage of the manure/slurry storage not equal to 100 percent"
merr3_desc <- "merr3_Indicated nutrient management but no area of grassland or cropland given"
merr4_desc <- "merr4_Sum of general crop rotation land and land not included in general crop rotation differs from total area item12 by 3 ha or more"
merr5_desc <- "merr5_Sum of area pH tested_ grassland and cropland_ is greater than total area item12"
merr6_desc <- "merr6_Sum of area nutrient management plan (grassland and cropland) is greater than total area item12"
merr7_desc <- "merr7_Sum of total farmed area fertilised with mineral and manure fertilisers greater than total area item12"

#module error list
merr_list <- paste0("merr", 1:7)





# Functions ---------------------------------------------------------------

#create dataframe of error names and descriptions
error_desc_df <- function(x){
  desc_list <- as.list(paste(x, "desc", sep= "_")) 
  names(desc_list) <- x 
  desc_list <- lapply(desc_list, function(x) get(x)) 
  bind_cols(desc_list)  %>% 
    pivot_longer(everything(), names_to ="error_name", values_to = "error_description")
  }

#remove total errors per case and ID col before joining
remove_total_id <- function(x) { x <-  x %>% select(-total_errors_per_case, -contains("ID", ignore.case = FALSE))  }

#function to remove columns where error = 0
remove_zero <- function(x) {
  dplyr::select(x, where( ~ any(. != 0)))
}


# Import ------------------------------------------------------------------

#main validation outputs

#summaries commented out to produce simple output for Ops. Coment back in if required

# v_summary_all <- read_table_from_db(server=server, 
#                                    database=database, 
#                                    schema=schema, 
#                                    table_name="JAC23_main_validation_summary")
# 
# v_summary_prioritised <- read_table_from_db(server=server, 
#                                             database=database, 
#                                             schema=schema, 
#                                             table_name="JAC23_main_validation_prioritised_summary")

v_list <- read_table_from_db(server=server, 
                              database=database, 
                              schema=schema, 
                              table_name="JAC23_main_validation_list")

v_np_list<- read_table_from_db(server=server, 
                               database=database, 
                               schema=schema, 
                               table_name="JAC23_main_validation_non_priority_list_full")

#context validation outputs
# 
# cv_summary <- read_table_from_db(server=server, 
#                                  database=database, 
#                                  schema=schema, 
#                                  table_name="JAC23_context_validation_summary")
# 
cv_list <- read_table_from_db(server=server, 
                              database=database, 
                              schema=schema, 
                              table_name="JAC23_context_validation_list") 
#module validation outputs
# 
# mv_summary <- read_table_from_db(server=server, 
#                                  database=database, 
#                                  schema=schema, 
#                                  table_name="JAC23_module_validation_summary")

mv_list <- read_table_from_db(server=server, 
                              database=database, 
                              schema=schema, 
                              table_name="JAC23_module_validation_list")





#Error descriptions into tables -----------------------------------
main_validations_desc <- error_desc_df(main_validation_list) 
all_context_desc <- error_desc_df(context_error_list)
all_module_validations_desc <- error_desc_df(merr_list)

# Join tables -------------------------------------------------------------

#remove total errors per case and ID column before join 
mv_list <- remove_total_id(mv_list)
cv_list <- remove_total_id(cv_list)
v_list <- remove_total_id(v_list)
v_np_list <- remove_total_id(v_np_list)

#remove columns of just zero before join
v_np_list <-v_np_list[colSums(v_np_list, na.rm = TRUE) !=0]
v_list <-v_list[colSums(v_list, na.rm = TRUE) !=0]
mv_list <-mv_list[colSums(mv_list, na.rm = TRUE) !=0]
cv_list <-cv_list[colSums(cv_list, na.rm = TRUE) !=0]

all_work_items <- full_join(v_list, v_np_list, by = c("parish", "holding"))
all_work_items <- full_join(all_work_items, mv_list, by = c("parish", "holding"))
all_work_items <- full_join(all_work_items, cv_list, by = c("parish", "holding"))
#all_work_items <- all_work_items %>% select(parish, holding, contains(c("total", "err")))
all_work_items$total_errors_per_case <- rowSums(all_work_items[-grep("_diff|parish|holding", names(all_work_items))], na.rm=TRUE)
all_work_items <- all_work_items %>% select(parish, holding, contains("err"), total_errors_per_case)


#set 0 as NA for clarity
all_work_items <- all_work_items %>% mutate(across(where(is.numeric), ~ ifelse(.==0, NA, .)))
#set priority?

# Export ADM --------------------------------------------------------------
#export df with simple names

write_dataframe_to_db(server=server,
                      database=database,
                      schema=schema,
                      table_name="JAC_Ops_work_item_list" ,
                      dataframe=all_work_items,
                      append_to_existing = FALSE,
                      versioned_table=FALSE,
                      batch_size = 10000)







# Export xlsx -------------------------------------------------------------

# 
# #change error column headings to error + description
# colnames(all_work_items)[colnames(all_work_items)%in% names(all_context_desc)] <- all_context_desc %>% select(any_of(names(all_work_items)))
# colnames(all_work_items)[colnames(all_work_items)%in% names(main_validations_desc)] <- main_validations_desc %>% select(any_of(names(all_work_items)))
# colnames(all_work_items)[colnames(all_work_items)%in% names(all_module_validations_desc)] <- all_module_validations_desc %>% select(any_of(names(all_work_items)))

#rename err60.x and err60.y as legal error
# all_work_items <- all_work_items %>% rename('err60_Legal responsbility box is not ticked (item2727) and no legal responsibility details (item2980) are given'= err60.x,
#                                             'only error=err60_Legal responsbility box is not ticked (item2727) and no legal responsibility details (item2980) are given' = err60.y)
# 

all_work_items <- all_work_items %>% rename('err60'= err60.x,
                                            'only error=err60' = err60.y)
all_desc <- bind_rows(main_validations_desc, all_context_desc, all_module_validations_desc)
#create holdings-ignored form to append to work item spreadsheet
#holdings_not_cleared_form <- data.frame(Parish = "", Holding = "", Errors_ignored = "",	Rationale ="", Date = "") 


# #change summaries to dataframes
# v_summary_all$error_description <- as.character(v_summary_all$error_description)
# v_summary_prioritised$error_description <- as.character(v_summary_prioritised$error_description)
# mv_summary$error_description <- as.character(mv_summary$error_description)
# cv_summary$error_description <- as.character(cv_summary$error_description)
#list of dataframes to export as a multi-sheet xlsx
# work_item_list <-list("work_items" = all_work_items, "main_error_summary" = v_summary_prioritised,  "main_errors_by_submisType" = v_summary_all, 
#                       "context_error_summary" = cv_summary, "module_error_summary" = mv_summary)

work_item_list <-list("work_items" = all_work_items,
                      "error_codes" = all_desc)
                        



all_error_descriptions <- 
#export  as xlsx for Ops
write_xlsx(work_item_list, paste(output_path, Sys.Date(), "work_item_list.xlsx"))

