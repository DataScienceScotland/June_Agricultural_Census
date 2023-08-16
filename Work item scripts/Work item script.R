


# Clearing holdings that Ops have looked at previously  -------------------

# Clear environment prior
rm(list = ls())

source("./Work item scripts/Initial all validation output script.R")


library(tidyverse)
library(RtoSQLServer)
library(writexl)
library(readxl)


#
# # Before import -----------------------------------------------------------
#
# # Datashare file path for import and export
# output_path <- "//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Codeconversion_2023/"
# #sas_agscens_path <- "//s0177a/sasdata1/ags/census/agscens/"
#
# #load(paste(output_path, "2023/Ags_A_2023.rda", sep = ""))
#
# # ADM schema for export
# server <- "s0196a\\ADM"
# database <- "RuralAndEnvironmentalScienceFarmingStatistics"
# schema <- "juneagriculturalsurvey2023alpha"
#
#
#
# Import latest work item list ----------------------------------------------
#
# all_work_items<- read_table_from_db(server=server,
#                                     database=database,
#                                     schema=schema,
#                                     table_name="JAC_Ops_work_item_list")
#



# Import form from previous work items list --------------------------------


non_cleared_holdings <-
  read_xlsx(paste0(output_path, "Checked holdings not cleared.xlsx")) %>%
  select(Parish, Holding) %>%
  rename(parish = Parish, holding = Holding)

# Remove non-cleared holdings from latest work item list -------
non_checked_holdings <-
  anti_join(all_work_items,
            non_cleared_holdings,
            by = c("parish", "holding"))


# #change error column headings to error + description
# colnames(non_checked_holdings)[colnames(non_checked_holdings) %in% names(all_context_desc)] <-
#   all_context_desc %>% select(any_of(names(non_checked_holdings)))
# colnames(non_checked_holdings)[colnames(non_checked_holdings) %in% names(main_validations_desc)] <-
#   main_validations_desc %>% select(any_of(names(non_checked_holdings)))
# colnames(non_checked_holdings)[colnames(non_checked_holdings) %in% names(all_module_validations_desc)] <-
#   all_module_validations_desc %>% select(any_of(names(non_checked_holdings)))


# Save Outputs ------------------------------------------------------------


#create holdings-ignored form to append to work item spreadsheet
#holdings_not_cleared_form <- data.frame(Parish = "", Holding = "", Errors_ignored = "",	Rationale ="", Date = "")

work_item_list <- list("work_items" = non_checked_holdings)

#write new xlsx with checked holdings removed
write_xlsx(work_item_list,
           paste(output_path, Sys.Date(), "work_item_list_dups_removed.xlsx"))
