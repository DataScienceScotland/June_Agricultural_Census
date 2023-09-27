
# June  - Validation Error Corrections ------------------------------------

#Script corrects errors from C2 validation checks  


# Clear environment prior
rm(list = ls())


# Load packages 

library(tidyverse)
library(RtoSQLServer)
library(writexl)
library(janitor)



#yr = this year 20xxR
yr <- 2023
yr1 <- yr - 1
yr2 <-  yr - 2
yr3 <-  yr - 3
yr_list <- c(yr, yr2, yr3)

# Before import -----------------------------------------------------------

source("C2 -  Validations/June - Main Validations.R") # this works for Lucy, if it doesn't work use next line

#source("C2 - Validations/June - Main Validations.R")

Code_directory <- ("//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Codeconversion_2023/2023")
# ADM schema for export
#server <- "s0196a\\ADM"

#database <- "RuralAndEnvironmentalScienceFarmingStatistics"
#schema <- "juneagriculturalsurvey2023alpha"

rm(combined_JAC,
   croft,
   legal_only_error,
   # brn_fruit,
   # check_labour,
   # check_labour_error_summary, 
   croft,
   fruit_holdings,
   #large_fruit,
   #non_priority_full,
   clean_JAC)



#Functions -----------------------------------------------------------

#calculate sum of (item11, item20026) and (rps area owned, rps area rented) and add to all_JAC_form 

#remove total errors per case and ID col before joining
remove_total_id <- function(x) { x <-  x %>% select(-total_errors_per_case, -contains("ID", ignore.case = FALSE))  }





# Section 1 (area) fixes ---------------------------------------------------------

#if area own (item11)  or area rented (item20026) = 0 and rps area own or rent >0, replace item11 and item20026 with rps areas

all_JAC_form<- all_JAC_form %>% mutate(
  item11 = case_when(item11 <= 0 & rps_totowned_june >0 |
                       is.na(item11)  & rps_totowned_june >0 ~rps_totowned_june,
                     TRUE ~as.numeric(item11)),
  item20026 = case_when(item20026 <= 0 & rps_totrented_june>0 |
                          is.na(item20026) & rps_totrented_june >0 ~rps_totrented_june, # LN added >0
                        TRUE~as.numeric(item20026)))

#if area own or area rented is greater than 0 but respective rps areas are zero, and sum of owned and rented not equal to sum of rps 
#areas or item12, replace with rps areas

total14 <- c(area_own, area_rent)
total14rps <- c(rps_own, rps_rent)
all_JAC_form$total14 <- rowSums(all_JAC_form[total14], na.rm = TRUE)
all_JAC_form$total14rps <-
  rowSums(all_JAC_form[total14rps], na.rm = TRUE)

all_JAC_form<- all_JAC_form%>% mutate(
  item11 = case_when((rps_totowned_june <= 0 & item11 >0 |
                       is.na(rps_totowned_june)  & item11 >0 ) &
                       abs(total14 - total14rps) <=3 &
                       total14 != item12 ~rps_totowned_june,
                     TRUE ~as.numeric(item11)),
  item20026 = case_when(item20026 <= 0 & rps_totrented_june>0 |
                          is.na(item20026) & rps_totrented_june >0 &
                          abs(total14 - total14rps) <=3 &
                          total14 != item12
                        ~rps_totrented_june, # LN added >0
                        TRUE~as.numeric(item20026)))




#if total area (item12) is zero and area owned and area rented is non zero AND
#difference between area owned + rented (total14) and RPS values (total14rps)  is less than or equal to 3 , 
#change item12 to rps_total area

total14 <- c(area_own, area_rent)
total14rps <- c(rps_own, rps_rent)
all_JAC_form$total14 <- rowSums(all_JAC_form[total14], na.rm = TRUE)
all_JAC_form$total14rps <-
  rowSums(all_JAC_form[total14rps], na.rm = TRUE)

all_JAC_form <- all_JAC_form %>% 
  mutate(item12= case_when(item12==0 & abs(total14 - total14rps) <=3
                           ~rps_totarea_june,
                           is.na(item12)& abs(total14 - total14rps)<=3
                           ~rps_totarea_june,
                           TRUE ~ as.numeric(item12)
  )
  )


#if item12 (total area) differs from area own + rented  and difference between area own + rented  RPS areas is
#less than or equal to 3 ha, change item12 to rps total area
total14 <- c(area_own, area_rent)
total14rps <- c(rps_own, rps_rent)
all_JAC_form$total14 <- rowSums(all_JAC_form[total14], na.rm = TRUE)
all_JAC_form$total14rps <-
  rowSums(all_JAC_form[total14rps], na.rm = TRUE)

all_JAC_form<- all_JAC_form %>% mutate(
  item12 = case_when(item12 != total14 &
                       abs(total14 - total14rps)<=3
                     ~ rps_totarea_june,
                     TRUE~as.numeric(item12))
)



#fix possible punch errors (decimal places in wrong place)
all_JAC_form <- all_JAC_form %>% mutate(item20026 = case_when(item20026 == 0.01 * rps_totrented_june|
                                                                item20026 == 0.1 * rps_totrented_june|
                                                                item20026 == 10 * rps_totrented_june |
                                                                item20026 == 100 * rps_totrented_june |
                                                                item20026 == 1000 * rps_totrented_june ~ rps_totrented_june,
                                                              rps_totrented_june == 0 & item20026 == item12 ~ rps_totrented_june,
                                                              TRUE ~ as.numeric(item20026))
)


#fix possible punch errors (decimal places in wrong place)
all_JAC_form <- all_JAC_form %>% mutate(item11 = case_when(item11 == 0.01 * rps_totowned_june|
                                                             item11 == 0.1 * rps_totowned_june|
                                                             item11 == 10 * rps_totowned_june |
                                                             item11 == 100 * rps_totowned_june |
                                                             item11 == 1000 * rps_totowned_june ~ rps_totowned_june,
                                                           rps_totowned_june == 0 & item11 == item12 ~ rps_totowned_june,
                                                           TRUE ~ as.numeric(item11))
)

#fix possible punch errors (decimal places in wrong place)
all_JAC_form <- all_JAC_form %>% mutate(item12 = case_when(item12 == 0.01 * rps_totarea_june|
                                                             item12 == 0.1 * rps_totarea_june|
                                                             item12 == 10 * rps_totarea_june |
                                                             item12 == 100 * rps_totarea_june |
                                                             item12 == 1000 * rps_totarea_june ~ rps_totarea_june,
                                                           TRUE ~ as.numeric(item12))
)


# Err61 correction --------------------------------------------------------
#count number of holdings with original err = 1
err61_original_count <- nrow(err61)

#run again to update error flag on all_JAC_form

#Sum of area owned and rented is not equal to sum of RP&S values by 30 or more hectares
#err61

total14 <- c(area_own, area_rent)
total14rps <- c(rps_own, rps_rent)
all_JAC_form$total14 <- rowSums(all_JAC_form[total14], na.rm = TRUE)
all_JAC_form$total14rps <-
  rowSums(all_JAC_form[total14rps], na.rm = TRUE)

all_JAC_form$err61 <-
  as.numeric(ifelse(abs(
    round(all_JAC_form$total14, digits = 2) - round(all_JAC_form$total14rps, digits = 2)
  ) >= 30,  1, 0))

all_JAC_form$err61_diff <-  as.numeric(ifelse(all_JAC_form$err61 == 1,
                                              abs(
                                                round(all_JAC_form$total14 - all_JAC_form$total14rps, digits = 2)
                                              ), 0))


#final holdings for Ops (after remaking err flag)
err61 <-
  all_JAC_form %>% select(
    parish,
    holding,
    survtype,
    submisType,
    land_data,
    saf_data,
    total14,
    total14rps,
    all_of(c(area_own, area_rent, rps_own, rps_rent)),
    err61,
    err61_diff
  )

err61 <-
  err61 %>%  filter(err61 ==1) %>% 
  mutate(err_fix= case_when(sum(item11, item20026, na.rm = TRUE) == sum(rps_totowned_june, rps_totrented_june) ~ "TRUE",
                            TRUE ~ "FALSE")) %>% 
  filter(err_fix =="FALSE")

err61_fix_count <- err61_original_count-nrow(err61)
# Err38 correction --------------------------------------------------------

#count number of holdings with original err = 1
err38_original_count <- nrow(err38)


#run again to update error flag on all_JAC_form

#Total area is zero and area owned and area rented =0
#err38
all_JAC_form$err38 <-
  as.numeric(ifelse(all_JAC_form[total_area] == 0 &
                      all_JAC_form[area_own] == 0 &
                      all_JAC_form[area_rent] == 0,
                    1,
                    0
  )
  )

#final holdings for Ops (after remaking err flag)

err38 <-
  all_JAC_form %>% select(
    parish,
    holding,
    survtype,
    submisType,
    land_data,
    saf_data,
    total14,
    total14rps,
    all_of(c(area_own, area_rent, total_area, rps_own, rps_rent, rps_total_area)),
    err38
    
  )

err38 <-
  err38 %>%  filter(err38 ==1) %>% 
  mutate(err_fix= case_when(err38 ==1 & 
                              item11 ==0 &
                              item20026== 0 & 
                              item12 ==0~ "FALSE",
                            TRUE ~ "TRUE"))

err38_fix_count <- err38_original_count-nrow(err38)

# Err37 correction --------------------------------------------------------

#count number of holdings with original err = 1
err37_original_count <- nrow(err37)

#run again to update error flag on all_JAC_form

#Total area is zero but area own or area rented >0
total14 <- c(area_own, area_rent)
total14rps <- c(rps_own, rps_rent)
all_JAC_form$total14 <- rowSums(all_JAC_form[total14], na.rm = TRUE)
all_JAC_form$total14rps <-
  rowSums(all_JAC_form[total14rps], na.rm = TRUE)

all_JAC_form$err37 <-
  as.numeric(ifelse((all_JAC_form[total_area] == 0 |
                       is.na(all_JAC_form[total_area])) &
                      sum(all_JAC_form[area_own], all_JAC_form[area_rent], na.rm=TRUE) > 0 &
                      #new change for re-run only: remove SAF_only data and ags data without any areas  
                      all_JAC_form$land_data =="ags" &
                      all_JAC_form$total14 !=0 &all_JAC_form$total14rps!=0,
                    1,
                    0))

#final holdings for Ops (after remaking err flag)
#err37 after err61 fix

err37 <-
  all_JAC_form %>% select(
    parish, 
    holding,
    survtype,
    submisType,
    land_data,
    saf_data,
    total14,
    total14rps,
    all_of(c(area_own, area_rent, total_area, rps_own, rps_rent, rps_total_area)),
    err37
    
  )

err37 <-
  err37 %>%  filter(err37 ==1)%>% 
  mutate(err_fix= case_when(err37 ==1 & 
                              item12 !=0 ~ "TRUE",
                            TRUE~"FALSE")
  )

err37_fix_count <- err37_original_count-nrow(err37)
# Err1 correction ---------------------------------------------------------

#count number of holdings with original err = 1
err1_original_count <- nrow(err1)


#run again to update error flag on all_JAC_form

#Total area not equal to area rent + area own (3 ha margin)

all_JAC_form$err1 <-
  as.numeric(ifelse(abs(
    round(all_JAC_form[total_area], digits = 2) - (round(all_JAC_form[area_own] +
                                                           all_JAC_form[area_rent], digits = 2))
  ) >= 3,  1, 0))
all_JAC_form$err1_diff <-  as.numeric(ifelse(all_JAC_form$err1 == 1,
                                             abs(rowSums((all_JAC_form[area_own] +all_JAC_form[area_rent]) -
                                                           all_JAC_form[total_area])), 0))

#check number of holdings this has fixed

err1 <-
  all_JAC_form %>% select(
    parish,
    holding,
    survtype,
    submisType,
    land_data,
    saf_data,
    all_of(c(area_own, area_rent, total_area, rps_total_area, rps_own, rps_rent)),
    total14,
    total14rps,
    err1,
    err1_diff
    
  )

err1 <-
  err1 %>% filter(err1 == 1) %>% 
  mutate(err_fix= case_when(item12 == sum(item11, item20026, na.rm = TRUE) ~ "TRUE",
                            TRUE ~ "FALSE"))


err1_fix_count <- err1_original_count-nrow(err1)



# Err43 correction --------------------------------------------------------

#count number of holdings with original err = 1
err43_original_count <- nrow(err43)

#run again to update error flag on all_JAC_form

#err43
all_JAC_form$err43 <-
  as.numeric(ifelse(round(all_JAC_form[seas_area_rent_out], digits = 2) - 
                      round(all_JAC_form[total_area], digits = 2) >=
                      3,
                    1,
                    0))

all_JAC_form$err43_diff <-   as.numeric(ifelse(all_JAC_form$err43 == 1,
                                               rowSums(all_JAC_form[seas_area_rent_out] -
                                                         all_JAC_form[total_area]),
                                               0))



#check number of holdings this has fixed

err43 <-
  all_JAC_form %>% select(
    parish,
    holding,
    survtype,
    submisType,
    land_data,
    saf_data,
    all_of(c(seas_area_rent_out, total_area, rps_total_area)),
    err43,
    err43_diff
  )

err43 <-
  err43 %>%   filter(err43==1) %>% 
  mutate(err43_diff = item12 - item1,
         err_fix = case_when(item1>item12 ~ "FALSE",
                             TRUE ~ "TRUE")
         
  )



err43_fix_count <- err43_original_count-nrow(err43)


# Err57 correction --------------------------------------------------------

#count number of holdings with original err = 1
err57_original_count <- nrow(err57)


#run again to update error flag on all_JAC_form

all_JAC_form$err57 <-
  as.numeric(ifelse(abs(
    round(all_JAC_form[area_rent], digits = 2) - round(all_JAC_form[rps_rent], digits = 2)
  ) >= 30, 1, 0))
all_JAC_form$err57_diff <-  as.numeric(ifelse(all_JAC_form$err57 == 1,
                                              rowSums(abs(all_JAC_form[area_rent] - all_JAC_form[rps_rent])), 0))


#check number of holdings this has fixed

err57 <-
  all_JAC_form %>% select(
    parish,
    holding,
    survtype,
    submisType,
    land_data,
    saf_data,
    all_of(c(rps_rent, area_rent, total_area, rps_total_area, area_own, rps_own )),
    err57,
    err57_diff
  )
err57 <-
  err57 %>%  filter(err57== 1) %>% 
  mutate(err57_diff = abs(item20026 - rps_totrented_june),
         err_fix = case_when(abs(item20026-rps_totrented_june) >30~ "FALSE",
                             TRUE ~ "TRUE")
         
  )



err57_fix_count <- err57_original_count-nrow(err57)


# Err59 correction --------------------------------------------------------

#count number of holdings with original err = 1
err59_original_count <- nrow(err59)


#run again to update error flag on all_JAC_form
all_JAC_form$err59 <-
  as.numeric(ifelse(abs(
    round(all_JAC_form[area_own], digits = 2) - round(all_JAC_form[rps_own], digits = 2)
  ) >= 30,  1, 0))
all_JAC_form$err59_diff <-  as.numeric(ifelse(all_JAC_form$err59 == 1,
                                              rowSums(abs(all_JAC_form[area_own] - all_JAC_form[rps_own])), 0))

#check number of holdings this has fixed

err59 <-
  all_JAC_form %>% select(
    parish,
    holding,
    survtype,
    submisType,
    land_data,
    saf_data,
    all_of(c(area_own, rps_own, rps_rent, area_rent, total_area, rps_total_area)),
    err59,
    err59_diff
  )
err59 <-
  err59 %>%  filter(err59== 1) %>% 
  mutate(err59_diff = abs(item11- rps_totowned_june),
         err_fix = case_when(abs(item11-rps_totowned_june) >30~ "FALSE",
                             TRUE ~ "TRUE")
         
  )


err59_fix_count <- err59_original_count-nrow(err59)



# Section 3 fixes ---------------------------------------------------------

#Ags: if total land (item50) is zero,  but total land use items (total13) is greater than 0, replace item50 with total13
#if total land not equal to total land use items AND holding is ags, replace item50 with total 13.

#SAF or both: if total land (item50) is zero, but total land use items (total7) is greater than 0, replace item50 with total7
#if total land not equal to total land use items AND holding is not ags, replace item50 with total 7 
#total 7 = total area of crops and grassland, rough grazing, woodland and other land) . 

all_JAC_form <- all_JAC_form %>% mutate(
  item50 = case_when(item50 ==0 & land_data =="ags" & total13 !=0 ~ total13,
                     item50 ==0 & land_data !="ags" & total7 !=0 ~ total7,
                     item50 !=total13 & land_data =="ags" ~ total13,
                     item50!=total7 & land_data !="ags" ~ total7,
                     TRUE ~ as.numeric(item50)
  )
)




#change item50 to item12 if item50 is zero and item12 = total13 or total7 (sum of all land items) 
all_JAC_form <- all_JAC_form %>% mutate(
  item50 = case_when((abs(item12 - item50)) > 3 &
                       item50 ==0 &
                       item12 ==total13 |
                       item12==total7
                     ~ item12,
                     TRUE ~ as.numeric(item50)
  )
)



# Err45 correction --------------------------------------------------------


#Re-run to update error flag on all_JAC_form

all_JAC_form$err45 <-
  as.numeric(ifelse(
    abs(all_JAC_form[total_land] - all_JAC_form$total13) >= 3 &
      all_JAC_form$land_data =="ags",
    1,
    0
  ))

all_JAC_form$err45_diff <-
  as.numeric(ifelse(all_JAC_form$err45 == 1,
                    abs(rowSums(all_JAC_form[total_land] - all_JAC_form$total13)), 
                    0))


#check number of holdings this has fixed
err45 <-
  all_JAC_form %>% select(
    parish,
    holding,
    survtype,
    submisType,
    land_data,
    saf_data,
    all_of(c(total_land, total_area, rps_total_area)),
    total13,
    err45,
    err45_diff,
    total7, 
  )

err45 <-
  err45 %>% filter(err45 == 1) %>% 
  mutate(err_fix = case_when (total13 == item50~ "TRUE",
                              TRUE~ "FALSE")
  )


#filter for holdings that might need further interventions
# err45_ops <- err45 %>%  filter(err_fix =="FALSE" ) %>%
#   mutate(item50eqtotal7 = case_when(
#     item50 == total7 ~1,
#     TRUE ~ 0
#   )
#   ) %>% filter(item50eqtotal7 !=1)

#count for ops
err45_fix_count <- nrow(err45) - nrow(err45) 


# Err46 correction --------------------------------------------------------

err_46_original_count <- nrow(err46)


#Re-run to update error flag on all_JAC_form

#Total at end of land use section equal to total area


all_JAC_form$err46 <-
  as.numeric(
    ifelse(
      abs(all_JAC_form[total_land] - all_JAC_form[total_area]) >= 3 &
        all_JAC_form[total_land]> all_JAC_form[total_area] &
        all_JAC_form$land_data =="ags",
      1,
      0
    ))

all_JAC_form$err46_diff <-
  as.numeric(ifelse(all_JAC_form$err46 == 1, 
                    abs(rowSums(all_JAC_form[total_land] - all_JAC_form[total_area])), 0))




#check number of holdings this has fixed
err46 <-
  all_JAC_form %>% select(
    parish,
    holding,
    survtype,
    submisType,
    land_data,
    saf_data,
    all_of(c(total_land, total_area, rps_total_area)),
    err46,
    err46_diff,
    total13,
    err45_diff,
    total7,
    item9999
  )

err46 <-
  err46 %>%  filter(err46 == 1) %>% 
  mutate(err_fix = case_when(item50 == item12 ~ "TRUE",
                             TRUE~ "FALSE"), 
         item50gtitem12 = case_when(item50>item12 ~"TRUE",
                                    TRUE ~ "FALSE")
  )


#add column to see if item50 is greater than item12 - these need to be checked by Ops
# <- err46 %>% 
  #filter(item50gtitem12 == "TRUE" & land_data== "ags")

#count for ops
err46_fix_count <- err_46_original_count - nrow(err46) 


# Err32 correction --------------------------------------------------------

#count number of holdings with original err = 1
err32_original_count <- nrow(err32)

#change total labour(item200) to total of labour items (total12) if item200<total12 

all_JAC_form <- all_JAC_form %>% mutate(
  item200 = case_when(item200<total12 ~ total12,
                      TRUE ~ as.numeric(item200)
  )
)

#Re-run to update error flag on all_JAC_form

all_JAC_form$err32 <-
  as.numeric(ifelse(round(all_JAC_form[total_labour], digits = 1) != all_JAC_form$total12, 1, 0))
all_JAC_form$err32_diff <-
  as.numeric(ifelse(all_JAC_form$err32 == 1, rowSums(abs(all_JAC_form[total_labour] - all_JAC_form$total12)), 0))
all_JAC_form$err32_diff <- abs(all_JAC_form$err32_diff)




#check number of holdings this has fixed
err32 <-
  all_JAC_form %>% select(parish,
                          holding,
                          survtype,
                          submisType,
                          land_data,
                          saf_data,
                          all_of(c(total12, total_labour)),
                          total12,
                          err32)
err32 <-
  err32 %>% filter(err32==1) %>% 
  mutate(err_fix = case_when(item200==total12 ~ "TRUE",
                             TRUE~ "FALSE")
  )



err32_fix_count <- err32_original_count-nrow(err32)


# Err11 correction --------------------------------------------------------



#count number of holdings with original err = 1
err11_original_count <- nrow(err11)


#change glasshouse totals (open soil = item85, solid floor = item86) to sum of glasshouse items (total5b/ total6b)
all_JAC_form <- all_JAC_form %>% 
  mutate(item85 = case_when(item85<total5b ~ total5b,
                            TRUE~ as.numeric(item85)
  ),
  item86 = case_when(item86<total6b~ total6b,
                     TRUE~ as.numeric(item86)
  )
  )

#Re-run to update error flag on all_JAC_form

all_JAC_form$err11 <-
  as.numeric(ifelse((
    round(all_JAC_form$total5b, digits = 2) != round(all_JAC_form[tot_open_plastic], digits = 2)
  ) |
    (
      round(all_JAC_form$total6b, digits = 2) != round(all_JAC_form[tot_solid_glass], digits = 2)
    ),  1, 0)
  )

all_JAC_form$err11open_diff <-
  as.numeric(ifelse(all_JAC_form$err11 == 1,
                    abs(rowSums(all_JAC_form[tot_open_plastic] - all_JAC_form$total5b)), 0))

all_JAC_form$err11solid_diff <-
  as.numeric(ifelse(all_JAC_form$err11 == 1, 
                    abs(rowSums(all_JAC_form[tot_solid_glass] - all_JAC_form$total6b)), 0))

#check number of holdings this has fixed
err11 <-
  all_JAC_form %>% select(
    parish,
    holding,
    survtype,
    submisType,
    land_data,
    saf_data,
    all_of(c(
      total5b, tot_open_plastic, total6b, tot_solid_glass
    )),
    err11,
    err11open_diff,
    err11solid_diff
  )

err11 <-
  err11 %>% filter(err11 == 1) %>% 
  mutate(err_fix_85 = case_when(item85==item27750 + item2862 ~"TRUE",
                                TRUE~ "FALSE"),
         err_fix_86 = case_when(item86 == item27755 + item2867 ~ "TRUE",
                                TRUE~ "FALSE")
  )



err11_fix_count <- err11_original_count-nrow(err11)


# Err12 correction --------------------------------------------------------

#count number of holdings with original err = 1
err12_original_count <- nrow(err12)

#Re-run to update error flag on all_JAC_form
all_JAC_form$err12 <-
  as.numeric(ifelse(
    all_JAC_form[tot_open_plastic] + all_JAC_form[tot_solid_glass] > 0 &
      (all_JAC_form$total5b + all_JAC_form$total6b) <= 0,
    1,
    0))
all_JAC_form$err12_diff <-
  as.numeric(ifelse(all_JAC_form$err12 == 1, abs(rowSums((all_JAC_form[tot_open_plastic] + all_JAC_form[tot_solid_glass]) -
                                                           (all_JAC_form$total5b + all_JAC_form$total6b)
  )), 0))


#err12 after err11 fix
err12 <-
  all_JAC_form %>% select(
    parish,
    holding,
    survtype,
    submisType,
    land_data,
    saf_data,
    all_of(c(
      tot_open_plastic, total5b, tot_solid_glass, total6b
    )),
    total5b,
    total6b,
    err12,
    err12_diff
  )


err12 <-
  err12 %>% filter(err12 == 1) %>% 
  mutate(err_fix = case_when((item85 + item86) > 0 & (total5b + total6b) <=0~"FALSE",
                             TRUE~ "TRUE")
         
  )


err12_fix_count <- err12_original_count-nrow(err12)



# Err13 correction --------------------------------------------------------

#count number of holdings with original err = 1
err13_original_count <- nrow(err13)



#change total pig (item157) to sum of pig items(total10) if item157 ==0
all_JAC_form <- all_JAC_form %>% 
  mutate(item157 = case_when(item157==0~ total10,
                             TRUE~ as.numeric(item157)
  )
  )

#Re-run to update error flag on all_JAC_form

#Total pigs is not equal to sum of individual pig items
all_JAC_form$err13 <-
  as.numeric(ifelse(round(all_JAC_form$total10, digits = 1) != round(all_JAC_form[total_pig], digits = 1),
                    1,
                    0))
all_JAC_form$err13_diff <-
  as.numeric(ifelse(all_JAC_form$err13 == 1,  abs(rowSums(all_JAC_form[total_pig] - all_JAC_form$total10)), 0))


#check number of holdings this has fixed
err13 <-
  all_JAC_form %>% select(
    parish,
    holding,
    survtype,
    submisType,
    land_data,
    saf_data,
    all_of(total_pig),
    total10,
    err13,
    err13_diff
  )

err13 <-
  err13 %>%  filter(err13 == 1) %>% 
  mutate(err_fix = case_when(item157 == total10 ~ "TRUE",
                             TRUE ~ "FALSE"))


err13_fix_count <- err13_original_count-nrow(err13)



#Section 5
# Err17 correction --------------------------------------------------------

#count number of holdings with original err = 1
err17_original_count <- nrow(err17)

#change total poultry (item170) to sum of poultry items(total11) if item170=0
all_JAC_form <- all_JAC_form %>% 
  mutate(item170 = case_when(item170==0 ~ total11,
                             TRUE~ as.numeric(item170)
  )
  )

#Re-run to update error flag on all_JAC_form

all_JAC_form$err17 <-
  as.numeric(ifelse(round(all_JAC_form$total11, digits = 1) != round(all_JAC_form[total_poultry], digits = 1),
                    1,
                    0)
  )
all_JAC_form$err17_diff <-
  as.numeric(ifelse(all_JAC_form$err17 == 1,  abs(rowSums(all_JAC_form[total_poultry] - all_JAC_form$total11)), 0))



#check number of holdings this has fixed

err17 <-
  all_JAC_form %>% select(
    parish,
    holding,
    survtype,
    submisType,
    land_data,
    saf_data,
    all_of(c(total11, total_poultry)),
    total11,
    err17,
    err17_diff
  )

err17 <-
  err17 %>%  filter(err17 == 1) %>% 
  mutate(err_fix= case_when(item170 == total11 ~ "TRUE",
                            TRUE ~ "FALSE"))



err17_fix_count <- err17_original_count-nrow(err17)



# Section 5 fixes ---------------------------------------------------------
# Err15 correction --------------------------------------------------------

#count number of holdings with original err = 1
err15_original_count <- nrow(err15)



#change total sheep (item145) to sum of sheep items(total9) if item145= 0
all_JAC_form <- all_JAC_form %>% 
  mutate(item145 = case_when(item145==0  ~ total9,
                             TRUE~ as.numeric(item145)
  )
  )

#Re-run to update error flag on all_JAC_form

all_JAC_form$err15 <-
  as.numeric(ifelse(round(all_JAC_form$total9, digits = 1) != round(all_JAC_form[total_sheep], digits = 1),
                    1,
                    0))
all_JAC_form$err15_diff <-
  as.numeric(ifelse(all_JAC_form$err15 == 1, abs(rowSums(all_JAC_form[total_sheep] - all_JAC_form$total9)), 0))


#check number of holdings this has fixed
err15 <-
  all_JAC_form %>% select(
    parish,
    holding,
    survtype,
    submisType,
    land_data,
    saf_data,
    all_of(c(total9, total_sheep)),
    total9,
    err15,
    err15_diff
  )

err15 <-
  err15 %>%  filter(err15==1) %>% 
  mutate(err_fix= case_when(item145 == total9 ~ "TRUE",
                            TRUE ~ "FALSE"))



err15_fix_count <- err15_original_count-nrow(err15)


# Err16 correction --------------------------------------------------------
#not automated- check which are whole or not and determine source of error.
#checks 
#round_check <- function(x){ x==round(x) }
#round_check(err16[sheep_vars])  

#count number of holdings with original err = 1
err16_original_count <- nrow(err16)


#exclude item144 to calculate difference remaining from sheep items and sheep totals, 
#if difference equals item144 x 10, change item144 to sheep_round_diff
#if difference is<3, change item144 to sheep_round_diff

all_JAC_form <- all_JAC_form %>%
  mutate(sheep_round_diff = item145-item139 - item140- item141 - item143,
         item144= case_when(sheep_round_diff == item144*10 ~item144*10,
                            abs(item144 - sheep_round_diff)<3 ~ sheep_round_diff,
                            TRUE ~as.numeric(item144)
         )
  )

#Re-run to update error flag on all_JAC_form

all_JAC_form$err16 <-
  as.numeric(ifelse(
    round(all_JAC_form[ewes], digits = 0) != all_JAC_form[ewes] |
      round(all_JAC_form[rams], digits = 0) != all_JAC_form[rams] |
      round(all_JAC_form[breed_other_sheep], digits = 0) != all_JAC_form[breed_other_sheep] |
      round(all_JAC_form[no_breed_other_sheep], digits = 0) != all_JAC_form[no_breed_other_sheep] |
      round(all_JAC_form[lambs], digits = 0) != all_JAC_form[lambs] |
      round(all_JAC_form[total_sheep], digits = 0) != all_JAC_form[total_sheep],
    1,
    0
  )
  )

#check number of holdings this has fixed
err16 <-
  all_JAC_form %>% select(parish,
                          holding,
                          survtype,
                          submisType,
                          land_data,
                          saf_data,
                          all_of(c(total9, total_sheep)),
                          sheep_round_diff,
                          total9,
                          err16)

err16 <-
  err16 %>%  filter(err16 == 1) %>% 
  mutate(err_fix= case_when(item144 == sheep_round_diff ~ "TRUE",
                            TRUE ~ "FALSE"))

#filter for holdings that might need further interventions
err16_ops<- err16 %>%  filter(err_fix =="FALSE")

err16_fix_count <- err16_original_count-nrow(err16)




#Validations- corrected summary--------------------------------------------------------------------------------------------------------------------

#dataframe of observations (holdings) that have failed the new validations -
JAC_validation_corrected_errors <-
  all_JAC_form %>% select(parish, holding, submisType, any_of(all_validations)) %>%  filter(if_any(starts_with("err"), ~ . !=
                                                                                                     0))
JAC_validation_corrected_error_summary <-
  JAC_validation_corrected_errors %>% ungroup() %>% group_by(submisType) %>%   select(starts_with("err")) %>% dplyr::summarize(across(everything(), sum, na.rm = TRUE))
JAC_validation_corrected_error_summary <-
  cbind(
    JAC_validation_corrected_error_summary,
    total_cases_with_errors = rowSums(JAC_validation_corrected_error_summary[names(JAC_validation_corrected_error_summary) %in% all_validations], na.rm =
                                        TRUE)
  )
JAC_validation_corrected_error_summary <-
  as.data.frame(t(JAC_validation_corrected_error_summary)) %>%
  janitor::row_to_names(1)
colnames(JAC_validation_corrected_error_summary) <-
  paste0("SubmisType", colnames(JAC_validation_corrected_error_summary))

#select error columns from dataset and filter for cases with errors
main_validations <-
  all_JAC_form %>% select(
    parish,
    holding,
    submisType,
    any_of(all_validations),
    err46_diff,
    err45_diff,
    err59_diff,
    err57_diff,
    err61_diff)%>% 
    #exclude croft error
    #-err34
  
  #filter to exclude saf_only 
  filter(submisType != "NA")

#list of relevant errors to filter
#relevant_errors <-
#names(main_validations[names(main_validations) %in% all_validations])

main_validations <-
  main_validations %>% filter(if_any(starts_with("err"), ~ . !=
                                       0))

#Work item hierarchy------------------------------------------------------

#include err46_diff, err_45_diff and err61_diff to arrange by largest discrepancies
#trimmed off err13, err15, err17, err32
priority <-
  c(
    "err46_diff",
    "err46",
    "err45_diff",
    "err45",
    "err4",
    "err44",
    "err24",
    "err47",
    "err49",
    "err50",
    "err30",
    "err31",
    "err60",
    "err54",
    "err53",
    "err48",
    "err51",
    "err55",
    "err11",
    "err1",
    "err37",
    "err38",
    "err61",
    "err61_diff",
    "err52"
  )

#arrange columns according to work item priority order
main_validations <-
  main_validations %>% select(parish, holding, any_of(priority), starts_with("err"))




#manual sorting rows by order of priority - todo: loop through priority list
#comment out errors that are zero - won't work otherwise.

main_validations <-
  main_validations %>% arrange(
    desc(err46),
    desc(err46_diff),
    desc(err45),
    desc(err45_diff),
    desc(err4),
    desc(err44),
    desc(err24),
    desc(err47),
    desc(err49),
    desc(err50),
    desc(err30),
    desc(err31),
    desc(err60),
    desc(err48),
    #desc(err51),
    desc(err55),
    desc(err1),
    desc(err37),
    desc(err38),
    desc(err61),
    desc(err61_diff)
  )
#desc(err52))



#count number of errors for each holding (exclude parish and holding, first two columns)

main_validations$total_errors_per_case <-
  rowSums(main_validations[names(main_validations) %in% all_validations], na.rm =
            TRUE)

# #filter where total error per case !=0
# main_validations <-
#   main_validations %>% filter(total_errors_per_case != 0)

#filter to select cases where err60 is the only error present
legal_only_error <-
  main_validations %>% filter(err60 == 1 & total_errors_per_case == 1)


#anti-join to remove cases where err60 is the only error present from total error cases
main_validations <-
  anti_join(main_validations, legal_only_error, by = c("parish", "holding"))



# prepare all_JAC_form to save as new combined_data  ----------------------

corrected_combined <- all_JAC_form %>% select(-starts_with(c("err", "merr", "total", "sheep")))


# load combined_data_2023, add crofts and overwrite with corrections 


combined <- read_table_from_db(
  server = server,
  database = database,
  schema = schema,
  table_name = "combined_data_2023"
)

croft <- read_table_from_db(
  server = server,
  database = database,
  schema = schema,
  table_name = "crofts_A_2023"
)

croft <- croft %>% mutate(croft = 1)

combined_croft <-
  left_join(combined, croft, by = c("parish", "holding")) %>%
  mutate(croft = case_when(croft == 1 ~ as.numeric(croft),
                           TRUE ~ 0))

combined_data_corrected<-rows_update(combined_croft, corrected_combined, by=c("parish", "holding"))


# #Save Outputs----------------------------------------------------------------------------------------------

#Uncomment as necessary
# #Save to ADM
#
#submisType summary
# write_dataframe_to_db(server=server,
#                       database=database,
#                       schema=schema,
#                       table_name="JAC23_main_validation_summary",
#                       dataframe=JAC_validation_error_summary,
#                       append_to_existing = FALSE,
#                       versioned_table=FALSE,
#                       batch_size = 10000)
# #
# #prioritised summary
#   write_dataframe_to_db(server=server,
#                         database=database,
#                         schema=schema,
#                         table_name="JAC23_main_validation_prioritised_summary",
#                         dataframe=prioritised_main_validation_summary,
#                         append_to_existing = FALSE,
#                         versioned_table=FALSE,
#                         batch_size = 10000)
#
#main validations

# this doesn't save currently, need to change structure - LN 26/09/23

write_dataframe_to_db(
  server = server,
  database = database,
  schema = schema,
  table_name = "JAC23_main_validation_list",
  dataframe = main_validations,
  append_to_existing = FALSE,
  versioned_table = FALSE,
  batch_size = 10000
)

# this doesn't save currently, need to change structure - LN 26/09/23

#non-priority main validations-full
write_dataframe_to_db(server=server,
                      database=database,
                      schema=schema,
                      table_name="JAC23_main_validation_non_priority_list_full",
                      dataframe=legal_only_error,
                      append_to_existing = FALSE,
                      versioned_table=FALSE,
                      batch_size = 10000)



#corrected all_JAC_form
write_dataframe_to_db(server=server,
                      database=database,
                      schema=schema,
                      table_name="combined_data_2023_corrected",
                      dataframe=combined_data_corrected,
                      append_to_existing = FALSE,
                      versioned_table=FALSE,
                      batch_size = 10000)


save(combined_data_corrected, file = paste0(Code_directory, "/combined_data_corrected.rda"))

#
# #migrant checks
#
# #main validations
# write_dataframe_to_db(server=server,
#                       database=database,
#                       schema=schema,
#                       table_name="JAC23_labour_validation_list",
#                       dataframe=migrant_errors,
#                       append_to_existing = FALSE,
#                       versioned_table=FALSE,
#                       batch_size = 10000)
#
#

#
#