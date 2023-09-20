

# JAC 23 Module response data exploration  --------------------------------

library(tidyverse)
library(RtoSQLServer)
library(writexl)


#Clean environment prior
rm(list = ls())

#Functions-------------------------------------------------------------------

#function to determine if entry is non-zero
recode_response <- function(x) {
  ifelse(x != 0, 1, 0)
}

#calculate mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#calculate number of responses in recoded module dataframe
recode_number_response <- function(x) {
  x %>%
    mutate(number_responses = rowSums(.))
}

# Before import -----------------------------------------------------------


# Datashare file path for import and export
output_path <-
  "//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Codeconversion_2023/"

# ADM schema for export
server <- "s0196a\\ADM"
database <- "RuralAndEnvironmentalScienceFarmingStatistics"
schema <- "juneagriculturalsurvey2023alpha"

#Core item list

#Section 1 - Area of location----------------------------------------------------------------------------------------------------------------------------
#Total area
total_area <- "item12"
rps_total_area <-  "rps_totarea_june"

#Module item list----------------------------------------------------------------------------------------------------------------------------------------------------


#Section 12 – Agricultural production methods---------------------------------------------------------------

rough_grazing_woodland_only <- "item20705"

#Crop_rotation and cropping land
crop_rotation <- "item2653"
not_crop_rotation <- "item2654"
arable_mix_with_legume <- "item2680"

#Soil cover on cropping land
autumn_winter_crops <- "item2358"
field_veg_potatoes	<- "item2359"
multi_annual	<- "item3027"
cover_legume	<- "item2360"
cover_non_legume	<- "item2365"
plant_residues_stubble	<- "item2361"
bare_soil	<- "item2362"

#Tillage
zero_tillage	<- "item2655"
reduced_tillage	<- "item2364"
inversion_tillage	<- "item2369"

#Irrigation
irrrigation	<- "item2638"

#Nutrient management
soil_testing_grass	<- "item3028"
soil_testing_crop	<- "item3029"
soil_testing_change_nutrient	<- "item3032"
area_ph_grass	<- "item3512"
area_ph_crop	<- "item3513"
nutrient_plan_update	<- "item3514"
nutrient_plan_grass	<- "item2656"
nutrient_plan_crop	<- "item2657"
grass_legume_sward	<- "item2681"
nitrogen_mixed_sward	<- "item2682"
manure_mixed_sward	<- "item2683"
protected_urea	<- "item2684"
precision_farming	<- "item2685"

section_12 <-
  c(
    rough_grazing_woodland_only,
    crop_rotation,
    not_crop_rotation,
    arable_mix_with_legume,
    autumn_winter_crops,
    field_veg_potatoes,
    multi_annual,
    cover_legume,
    cover_non_legume,
    plant_residues_stubble,
    bare_soil,
    zero_tillage,
    reduced_tillage,
    inversion_tillage,
    irrrigation,
    soil_testing_grass,
    soil_testing_crop,
    soil_testing_change_nutrient,
    area_ph_grass,
    area_ph_crop,
    nutrient_plan_update,
    nutrient_plan_grass,
    nutrient_plan_crop,
    grass_legume_sward,
    nitrogen_mixed_sward,
    manure_mixed_sward,
    protected_urea,
    precision_farming
  )

#Section 13 – Manure, slurry and mineral fertiliser application-------- ------------------------------------------------

ghouse_fallow_woodland_oland_only <- "item21207"

#Area fertilised
fertiliser_mineral_area	<- "item5100"
fertiliser_organic_area	<- "item5101"

#Solid manure use and import/export
manure_use	<- "item5129"
solid_manure_export	<- "item5104"
solid_manure_import <- "item5105"

#Slurry/liquid manure use and import/export
slurry_used	<- "item5102"
slurry_export	<- "item5106"
slurry_import	<- "item5107"

#Mineral fertiliser and other organic fertilisers
mineral_fertilisers_spread	<- "item5130"
organic_fertilisers_spread	<- "item5103"


#What percentage of the manure/slurry spread on your holding is spread using the following techniques?
broadcast_spreader_less_four_hours <- "item5108"
broadcast_spreader_more_four_hours	<- "item5109"
broadcast_spreader_not_ploughed	<- "item5110"
band_spreader_hose	<- "item5111"
band_spreader_shoe	<- "item5112"
open_slot_shallow_injection	<- "item5113"
closed_slot_deep_injection	<- "item5114"
other_spread	<- "item5131"

#Please indicate the percentage of total manure or slurry stored (including that eventually exported) in the following systems
storage_heaps	<- "item5115"
storage_compost	<- "item5116"
storage_pits_below_animals	<- "item5117"
storage_deep_litter	<- "item5118"
storage_without_cover	<- "item5119"
storage_permeable_cover	<- "item5120"
storage_impermeable_cover	<- "item5121"
storage_other_facilities	<- "item5122"
daily_spread	<- "item5123"
other_systems	<- "item5132"

#Please enter the number of months for which manure produced on the holding can be held, without risk of runoff, and without occasional emptying.
manure_compost_months	<- "item5124"
manure_pits_months	<- "item5125"
manure_deep_litter_months	<- "item5126"
slurry_months	<- "item5127"
manure_other_facilities_months	<- "item5128"

#Manure (over the last 12 months)
tested_nutrient_value	<- "item2662"
separate_applications	<- "item2663"

section_13 <-
  c(
    ghouse_fallow_woodland_oland_only,
    fertiliser_mineral_area,
    fertiliser_organic_area,
    manure_use,
    solid_manure_export,
    solid_manure_import,
    slurry_used,
    slurry_export,
    slurry_import,
    mineral_fertilisers_spread,
    organic_fertilisers_spread,
    broadcast_spreader_less_four_hours,
    broadcast_spreader_more_four_hours,
    broadcast_spreader_not_ploughed,
    band_spreader_hose,
    band_spreader_shoe,
    open_slot_shallow_injection,
    closed_slot_deep_injection,
    other_spread,
    storage_heaps,
    storage_compost,
    storage_pits_below_animals,
    storage_deep_litter,
    storage_without_cover,
    storage_permeable_cover,
    storage_impermeable_cover,
    storage_other_facilities,
    daily_spread,
    other_systems,
    manure_compost_months,
    manure_pits_months,
    manure_deep_litter_months,
    slurry_months,
    manure_other_facilities_months,
    tested_nutrient_value,
    separate_applications
  )


# Import ------------------------------------------------------------------

#combined dataset (SAF and JAC data)


combined_JAC <- read_table_from_db(
  server = server,
  database = database,
  schema = schema,
  table_name = "combined_data_2023"
)


combined_JAC <- combined_JAC %>% group_by(parish, holding)

#filter for only form returns
all_JAC_form <- combined_JAC %>% filter(survtype != "SAF_only")

JAC_module_23 <-
  all_JAC_form %>% select(parish, holding, submisType,
                          land_data, saf_data, all_of(c(
                            section_12, section_13, total_area, rps_total_area
                          )))
#recode information -------------------------------------------------------------------
#for info
#0 = Missing answer (NAs converted to 0 in B2 )
#1= Yes
#2 = No
#3 = NA (answered as NA on form)

# Data explorer -----------------------------------------------------------


#data classes
sapply(JAC_module_23, function(x)
  class(x))
#range of values
sapply(JAC_module_23, function(x)
  range(x))


#create df with just module variables
JAC_module_23 <-
  all_JAC_form %>% select(parish,
                          holding,
                          submisType,
                          land_data,
                          saf_data,
                          rps_totarea_june,
                          all_of(c(section_12, section_13, total_area)))




#create dataframe to recode module responses: 1 = response, 0 = no response
module_recode <-
  as.data.frame(JAC_module_23 %>% ungroup %>% select(all_of(c(
    section_12, section_13
  ))) %>% recode_response())

module_recode_stats <- module_recode %>%
  mutate(
    number_responses = rowSums(.),
    max_response_per_holding_section12 =
      ifelse(get(rough_grazing_woodland_only) == 1, 1, length(section_12)),
    max_response_per_holding_section13 =
      ifelse(
        get(ghouse_fallow_woodland_oland_only) == 1,
        1,
        length(section_13)
      ),
    total_max_response = (
      max_response_per_holding_section12 + max_response_per_holding_section13
    ),
    actual_vs_max = (number_responses / length(c(
      section_12, section_13
    ))) * 100,
    
  )



#check number of holdings with 0 responses across module
no_response <- module_recode %>%  recode_number_response() %>%
  filter(number_responses == 0) %>%
  nrow(.)

#check number of holdings with more than 1 response per section
response_section_12 <-
  module_recode %>% select(-all_of(c(section_13, rough_grazing_woodland_only))) %>%
  recode_number_response() %>%
  filter(number_responses >= 1) %>%
  nrow(.)

response_section_13 <-
  module_recode %>% select(-all_of(c(section_12, ghouse_fallow_woodland_oland_only))) %>%
  recode_number_response() %>%
  filter(number_responses >=1) %>%
  nrow(.)

#check number of holdings with responses in both sections
response_all_sections <-  
  module_recode %>% select(-all_of(c(rough_grazing_woodland_only, ghouse_fallow_woodland_oland_only))) %>%
  recode_number_response() %>%
  filter(number_responses >=1) %>%
  nrow(.)

#check number of holdings with more than 2 responses in module
response_more_2 <- module_recode %>%
  recode_number_response() %>%
  filter(number_responses > 2) %>%
  nrow(.)

#check number of holdings answering 1/3 module  or more
response_third <-  module_recode %>%
  recode_number_response() %>%
  filter(number_responses >= length(c(section_12, section_13)) / 3) %>%
  nrow(.)

#check number of holdings answering 1/2 module  or more
response_half <-  module_recode %>%
  recode_number_response() %>%
  filter(number_responses >= length(c(section_12, section_13)) / 2) %>%
  nrow(.)



#check number of holdings in total
response_module <- nrow(JAC_module_23)

#check number of holdings that have submitted maximum number of responses recorded
response_max <-  module_recode %>%
  recode_number_response() %>%
  filter(number_responses == max(number_responses)) %>%
  nrow(.)

#check number of holdings that have submitted mode number of responses recorded
response_mode <- module_recode %>%
  recode_number_response() %>%
  filter(number_responses == getmode(number_responses)) %>%
  nrow(.)



# Summary tables ----------------------------------------------------------


#summary grouped by tick boxes
recode_grouped_summary <-
  module_recode_stats %>% group_by(get(rough_grazing_woodland_only),
                                   get(ghouse_fallow_woodland_oland_only)) %>%
  summarise(
    total_holdings = n(),
    percent_total_holdings = (n() / response_module) * 100,
    average_module_response_per_holding = mean(number_responses),
    average_response_percent = mean(actual_vs_max),
    mode_response_per_holding = getmode(number_responses),
    max_response = max(number_responses),
    min_response = min(number_responses)
    %>% t()
    
  )

#rename grouping columns
recode_grouped_summary <-
  recode_grouped_summary %>% rename(section_12_answer = `get(rough_grazing_woodland_only)`,
                                    section_13_answer = `get(ghouse_fallow_woodland_oland_only)`)

#ungrouped summary
recode_summary <-
  module_recode_stats %>% summarise(
    max_possible_number_response_per_holding = length(c(section_12, section_13)),
    max_actual_response_per_holding = max(number_responses),
    number_of_holdings_max_response = response_max,
    max_actual_response_section_12 =  max(max_response_per_holding_section12),
    max_actual_response_section_13 = max(max_response_per_holding_section13),
    average_response_per_holding = mean(number_responses),
    average_response_rate_percent = (mean(number_responses) /
                                       length(c(
                                         section_12, section_13
                                       ))) * 100,
    mode_response_per_holding = getmode(number_responses),
    number_of_holdings_mode_response = response_mode,
    percent_mode_response = (response_mode /
                               response_module) * 100,
    number_of_holdings_no_response = no_response,
    percent_no_response = (no_response /
                             n()) * 100,
    number_of_holdings_1_more_section_12 = response_section_12,
    percent_1_more_section_12 = (response_section_12 /
                                   response_module) * 100,
    number_of_holdings_1_more_section_13 = response_section_13,
    percent_1_more_section_13 = (response_section_13 /
                                   response_module) * 100,
    number_of_holdings_section_12_13 = response_all_sections,
    percent_section_12_13 = (response_all_sections/
                                   response_module) * 100,
    number_of_holdings_2_more_module =
      response_more_2,
    percent_2_more_module = (response_more_2 /
                               response_module) * 100,
    number_of_holdings_third_module =
      response_third,
    percent_third_more_module = (response_third /
                                   response_module) * 100,
    number_of_holdings_half_module =
      response_half,
    percent_half_more_module = (response_half /
                                  response_module) * 100,
    total_holdings = n()
  ) %>%
  t() %>%
  as.data.frame()

recode_summary$stat <- row.names(recode_summary)
recode_summary <- recode_summary %>% select(stat, V1)

#recode submisType summary

module_recode_submis <-
  JAC_module_23 %>% ungroup %>% select(submisType, all_of(c(section_12, section_13)))
module_recode_submis[names(module_recode_submis) %in% c(section_12, section_13)] <-
  recode_response(module_recode_submis[names(module_recode_submis) %in% c(section_12, section_13)])


module_recode_submis <- module_recode_submis %>%
  mutate(
    number_responses = rowSums(module_recode_submis[names(module_recode_submis) %in% c(section_12, section_13)]),
    max_response_per_holding_section12 =
      ifelse(get(rough_grazing_woodland_only) == 1, 1, length(section_12)),
    max_response_per_holding_section13 =
      ifelse(
        get(ghouse_fallow_woodland_oland_only) == 1,
        1,
        length(section_13)
      ),
    total_max_response = (
      max_response_per_holding_section12 + max_response_per_holding_section13
    ),
    actual_vs_max = (number_responses / length(c(
      section_12, section_13
    ))) * 100,
    
  )



recode_submis_summary <-
  module_recode_submis %>% 
  group_by(submisType) %>%  
  summarise(
    total_holdings = n(),
    percent_total_holdings = (n() / response_module) * 100,
    average_module_response_per_holding = mean(number_responses),
    average_response_percent = mean(actual_vs_max),
    mode_response_per_holding = getmode(number_responses),
    max_response = max(number_responses),
    min_response = min(number_responses)
    %>% t()
    
  )


#number of responses for each item number
module_number_responses <-
  colSums(module_recode[c(section_12, section_13)])
module_number_responses <- as.data.frame(module_number_responses)
module_number_responses$item_number <-
  row.names(module_number_responses)
module_number_responses <-
  module_number_responses %>% select(item_number, module_number_responses)
module_number_responses$percent_response_holdings <-
  (module_number_responses$module_number_responses / nrow(module_recode)) *
  100


module_summary_list <-
  list(
    "item_response_count" = module_number_responses,
    "Summary_stats_overall" = recode_summary,
    "Section_grouped_summary" = recode_grouped_summary,
    "SubmisType_grouped_summary" = recode_submis_summary
  )

# Save Output -------------------------------------------------------------

#write new xlsx with checked holdings removed
write_xlsx(module_summary_list,
           paste(output_path, Sys.Date(), "JAC23_module_summary.xlsx"))
