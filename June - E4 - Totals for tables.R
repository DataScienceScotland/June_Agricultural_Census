##Written by Lucy Nevard 04/10/23
##Modified by Lucy Nevard 04/10/23



rm(list = ls())

# Before Import -----------------------------------------------------------

# Load packages and directories

library(readxl)
library(tidyverse)
library(stringr)
library(janitor)
library(data.table)
library(skimr)
library(haven)
library(RtoSQLServer)

source("item_numbers.R")


server <- "s0196a\\ADM"
database <- "RuralAndEnvironmentalScienceFarmingStatistics"
schema <- "juneagriculturalsurvey2023alpha"  # schema will change each year and need updating here. The table names can therefore stay the same 2024 onwards (will need editing below).


Code_directory <- ("//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Codeconversion_2023/2023")
sas_agstemp_path <- "//s0177a/sasdata1/ags/census/agstemp/"
sas_agscens_path <- "//s0177a/sasdata1/ags/census/agscens/"


jac <- read_table_from_db(server=server, 
                             database=database, 
                             schema=schema, 
                             table_name="JAC23_final_dataset")


# jactotals_111023 <- read_table_from_db(server=server, 
#                           database=database, 
#                           schema=schema, 
#                           table_name="JAC23_final_dataset_totals")



jac$id<-paste0(jac$parish,"_",jac$holding)

jac$yr<-2023

# Add late returns manually

h6710046<-jac %>% 
  filter(id=="671_46") %>% 
  mutate(item11=509.35,
         item20026=0,
         item12=509.35,
         item2827=306,
         item2828=19.22,
         item2=325.22,
         item1=0,
         item2321=44.06,
         item2322=318.25,
         item47=468.91,
         item48=3.35,
         item50=834.57,
         item139=405,
         item140=20,
         item141=40,
         item143=0,
         item144=615,
         item145=1080,
         item2980=2,
         item2877=1,
         item2878=0,
         item27785=1964,
         item177=1,
         item178=0,
         item179=0,
         item2566=0,
         item3056=0,
         item3057=0,
         item182=0,
         item183=0,
         item184=0,
         item2567=0,
         item1714=0,
         item1715=0,
         item1716=1,
         item1717=0,
         item192=0,
         item193=0,
         item1718=0,
         item194=0,
         item195=0,
         item1719=1,
         item196=0,
         item197=0,
         item198=0,
         item199=0,
         item200=2,
         item3028=1,
         item3029=2,
         item3032=2,
         item3512=13,
         item3514=2,
         item2656=0,
         item2657=0,
         item2681=44.06,
         item2682=12.3,
         item2683=2500,
         item2684=2,
         item2685=2,
         item5100=136,
         item5101=100,
         item5129=2500,
         item5130=62.5,
         item5110=100,
         item5116=100,
         item5124=6,
         madeup=0,
         saf_madeup=0,
         ags_madeup=0,
         survtype="Non-SAF",
         imptype="none",
         survdata="full",
         land_data="both"
         )


h1840008<-jac %>% 
  filter(id=="184_8") %>% 
  mutate(item11=8.75,
         item20026=0,
         item12=8.75,
         item2827=0,
         item2828=0,
         item2=0,
         item1=0,
         item2321=8.35,
         item2322=0,
         item47=0,
         item48=0,
         item50=8.75,
         item2727=1,
         item3028=2,
         item3032=3,
         item3512=0,
         item3514=2,
         item2656=0,
         item2681=0,
         item2682=0,
         item2683=0,
         item2684=2,
         item2685=2,
         item5100=0,
         item5101=0,
         item5129=0,
         item5104=0,
         item5105=0,
         item5102=0,
         item5106=0,
         item5107=0,
         item5130=0,
         item5103=0,
         item5108=0,
         item5109=0,
         item5110=0,
         item5111=0,
         item5112=0,
         item5113=0,
         item5114=0,
         item5131=0,
         item5115=0,
         item5116=0,
         item5117=0,
         item5118=0,
         item5119=0,
         item5120=0,
         item5121=0,
         item5122=0,
         item5123=0,
         item5132=0,
         item5124=0,
         item5125=0,
         item5126=0,
         item5127=0,
         item5128=0,
         item2662=2,
         item2663=2,
         madeup=0,
         saf_madeup=0,
         ags_madeup=0,
         survtype="Non-SAF",
         imptype="none",
         survdata="full",
         land_data="both"
  )


jac<-rows_update(jac, h6710046, by="id")

jac<-rows_update(jac, h1840008, by="id")


# Poultry corrections - roll forward biggest missing holdings



jac$poultry_rf=0


poultry<-data.frame(
  id=c("376_9","490_58","502_132","95_11","855_49","669_13","187_49","187_18","358_7","258_44","109_19","593_56","475_5","92_32","743_20","743_29","743_30","218_2","859_57","477_94","312_102","866_13","559_37","643_56","413_92","365_7","329_20","470_24"),
  poultry_rf=rep(1),
  item158=c(0,0,0,0,147600,0,128000,98400,96000,0,0,48000,40000,0,32000,32000,32000,32000,30000,29525,27763,0,0,18963,16087,0,12000,12000),
  item159=rep(0),
  item161=c(rep(0,13),32216, rep(0, 14)),
  item160=c(rep(0,21), 20700,20000,0,0,9931,0,0),
  item162=rep(0),
  item163=c(rep(0,21),2100,2000,0,0,3670,0,0),
  item164=c(344807,258000,169788,158976,0,135000,0,0,0,67068,56778,rep(0,17)),
  item170=c(344807,258000,169788,158976,147600,135000,128000,98400,96000,67068,56778,48000,40000,32216,32000,32000,32000,32000,30000,29525,27763,22800,22000,18963,16087,13601,12000,12000)
  
  )

jac<-rows_update(jac, poultry, by="id")

# Exclude holdings 


# Exclude outdoor experience centre - shouldn't be on the Census...


jac<-jac %>% 
  filter(id!="494_36")


jac<-jac %>% 
  filter(id!="161_44")

# Corrections 18.10.23

jac<-jac %>%
  dplyr::mutate(
    item171 = ifelse(id=="505_64", 0, 
                     ifelse(id=="406_4", 0, item171)),
    item1719= ifelse(id=="95_28", 0, item1719),
    item200= ifelse(id=="95_28", 100, 
                    ifelse(id=="360_47", 0,
                    ifelse(id=="514_21", 9, item200))),
    item1718=ifelse(id=="360_47", 0,
                    ifelse(id=="514_21", 2, item1718)),
    item1717= ifelse(id=="514_21", 1, item1717))
  

# correct complete data 


checkitem50<-jac %>% 
  select(id, item50, completedata, land_data) %>% 
  filter(item50>0 & completedata==0) %>% 
  mutate(completedata=1)

jac<-rows_update(jac, checkitem50, by="id")

# add survtype and completedata from 2021

jac2021 <- read_table_from_db(server=server,
                              database=database,
                              schema=schema,
                              table_name="jac_2021_update")


add2021<-jac2021 %>% 
  select(parish, holding, completedata, survtype) %>% 
  rename(completedata_2021=completedata,
         survtype_2021=survtype)


add2021$id<-paste0(add2021$parish,"_",add2021$holding)

add2021<-add2021 %>% 
  select(id, completedata_2021, survtype_2021)

add2021<-add2021[add2021$id %in% jac$id,]


jac<-left_join(jac, holdingscomplete2021, by="id")


  

land_items<-c("item27710", "item27715", "item27720", "item27725", "item27730", "item27735", "item27740", "item27750", "item27755", 
              "item16", "item18", "item17", "item20", "item19", "item23", 
              "item29", "item30", "item31", "item2059", "item32", "item34",
              "item52", "item53", "item55", "item56", "item2323", "item59", "item60", "item61", "item63", "item64", "item65", "item66",
              "item36", "item70", "item71", "item72", "item2832", "item75", 
              "item2324", "item1709", "item80", "item81", "item1710", "item82", "item83",
              "item2713", "item2858", "item2859", "item2860", "item2861", "item2714", "item2715", "item2716", "item2717", 
              "item2707", "item2863", "item2864", "item2865", "item2866", "item2708", "item2709", "item2710", "item2711")

livestock_items<-c("item27760", "item27775", "item27780", "item152", "item153", "item154", "item155", "item95", "item96", "item1712", "item1713", "item98")


# Load previous years

# previous_years <- read_table_from_db(server=server,
#                                      database=database,
#                                      schema=schema,
#                                      table_name="jac_previous_data_ten_years")


load(paste0(Code_directory, "/previous_years.rda"))

# re-make sums

jac<-jac %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(item12 = sum(item20026+item11, na.rm=TRUE), # total area of holding
                item2= ifelse(sum(item2827,item2828,item2879, na.rm=TRUE)>0, sum(item2827,item2828,item2879, na.rm=TRUE), item2), # seasonally rented in land
                item68 = ifelse(is.na(item68), sum(item52, item53, item55, item56, item2323, item59, item60, item61, item63, item64, item65, item66, na.rm=TRUE), item68), # total vegetables
                item35=ifelse(is.na(item35), item68, item35),
                item37 = item76,
                item27740 = ifelse(land_data=="saf", item84, item27740),
                item84=ifelse(is.na(item84), item27740, item84),
                item85=ifelse(imptype=="full", sum(item27750,item2862, na.rm=TRUE), item85),
                item86=ifelse(imptype=="full", sum(item27755,item2867, na.rm=TRUE), item86),
                item38 = sum(item41,item84,item85,item86, na.rm = TRUE),
                item40 = sum(item14, item15, item16, item18, item17, item20, item3156, item19, 
                             item23, item21, item24, item2320, item27, item28, item2034, item29, item30,
                             item31, item2059, item32, item34, item36, item2469, item2470, item35, item37, item38, na.rm=TRUE),
                item46 = sum(item2321, item2322, item40, na.rm=TRUE),
                item50 = sum(item46, item47, item48, item49, na.rm=TRUE),
                item145=sum(item139, item140, item143, item141, item144, na.rm=TRUE),
                item157 = sum(item146, item147,item148, item149,item150, item151, item27760,item27770, item27765, na.rm=TRUE),
                item170 = sum(item158, item159, item160, item161, item162, item163, item164, item167, item1708, item2038, item2039, na.rm=TRUE),
                item200=sum(item1714, item1715, item1716, item1717, item192, item193, item1718, item194, item195, item1719, item196, item197, item198, item199, na.rm=TRUE),
                item2556 = sum(item2858, item2863, na.rm=TRUE), # no solid floor strawberries from SAF
                item2557 = sum(item2859, item2864, na.rm=TRUE),# no solid floor raspberries from SAF
                item2836 = sum(item2860, item2865, na.rm=TRUE),# no solid floor blueberries from SAF
                item6001 = sum(item2862, item2867, na.rm=TRUE)
  )



checkglasshouseopen<-jac %>% 
  select(id, item27750, item2713, item2858, item2859, item2860, item2861, item2714, item2715, item2716, item2717,
         ) %>% 
  filter(item27750!=sum(item2713, item2858, item2859, item2860, item2861, item2714, item2715, item2716, item2717, na.rm=TRUE)) %>% 
  mutate(diff=item27750-sum(item2713, item2858, item2859, item2860, item2861, item2714, item2715, item2716, item2717, na.rm=TRUE))


checkglasshouseclosed<-jac %>% 
  select(id, item27755,
         item2707, item2863, item2864, item2865, item2866, item2708, item2709, item2710, item2711) %>% 
  filter(item27755!=sum(item2707, item2863, item2864, item2865, item2866, item2708, item2709, item2710, item2711, na.rm=TRUE)) %>% 
  mutate(diff=item27755-sum(item2707, item2863, item2864, item2865, item2866, item2708, item2709, item2710, item2711, na.rm=TRUE))


# Make totals -------------------------------------------------------------



jac<-jac %>%
  dplyr::rowwise() %>% 
  dplyr::mutate(
    item87=sum(item2713,item2707, na.rm=TRUE), #bear in mind 2713 isn't on SAF so could be an underestimate (made from disaggregating ags-only and imputed data)
    item39=sum(item2469,item2470, na.rm=TRUE),
    item6000=sum(item2861, item2866, na.rm=TRUE),
    item2036=sum(item2714, item2708,na.rm=TRUE),
    item2037=sum(item2715, item2709, na.rm=TRUE),
    item1711=sum(item2716, item2710, na.rm=TRUE),
    item1943=sum(item2717, item2711, na.rm=TRUE))

checkglasshouse<-jac %>% 
select(id, item27750, item2713, item2858, item2859, item2860, item2861, item2714, item2715, item2716, item2717, 
       item27755, item2707, item2863, item2864, item2865, item2866, item2708, item2709, item2710, item2711) %>% 
  dplyr::rowwise() %>% 
  filter(item27755>0 & sum(item2707, item2863, item2864, item2865, item2866, item2708, item2709, item2710, item2711,na.rm=TRUE)==0)


# Make totals based on E7 of SAS code -------------------------------------------------------------

# make items 

jac_disag<-jac %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(
                lambs=item144,
                ewes=item139,
                pigs20=item27770,
                pigs50=sum(item155,item27765, na.rm=TRUE), # item155 is disaggregated from 27760 but weaners are a separate category 27765
                pigs80=item154,
                tpigsbig=sum(item153,item152, na.rm=TRUE),
                pigsbig=tpigsbig,
                tothpigs=sum(item157-sum(item27760, item27765, item27770, na.rm=TRUE), na.rm=TRUE),
                othpigs=tothpigs,
                broilers=item164,
                tlayers=sum(item158,item159, na.rm=TRUE),
                layers=tlayers,
                peas=item52,
                cala=item60,
                turswe=item56,
                carrots=item63,
                #tveg=sum(item68,item36,item37, na.rm=TRUE) - sum(item52,item60,item56,item63, na.rm=TRUE),  # not sure this makes sense, produces some negative numbers...
                #veg=tveg,
                tothcer=sum(item14,item15,item17,item20, na.rm=TRUE), # no item22
                othcer=tothcer)


jac_disag<-jac_disag %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(
    otherfr_open=sum(item75,item72, na.rm=TRUE), # note from SAS: include blackcurrants in open (blackcurrants under cover are disclosive)
    otherfr_covered=sum(item2036,item6000, na.rm=TRUE),
    strawb = sum(item2556,item70, na.rm=TRUE),
    raspb = sum(item2557,item71, na.rm=TRUE),
    blackc = sum(item6000,item72, na.rm=TRUE),
    blueb = sum(item2836,item2832, na.rm=TRUE),
    tomato = sum(item87, na.rm=TRUE),
    otherfr = sum(item75,item2036, na.rm=TRUE),
    fruit = sum(strawb,raspb,blackc,blueb,tomato,otherfr, na.rm=TRUE),  # SAS code also includes mixedfr here but can't see how it's made. Mixed fruit is item75 which is already in othfr
    fewes=sum(ewes,lambs, na.rm=TRUE), # note didn't make fsheep=sum(sheep,fewes), not in the tables
    fpigs50=sum(pigs50,pigs20, na.rm=TRUE),
    fpigs80=sum(fpigs50,pigs80, na.rm=TRUE),
    fpigsbig=sum(pigsbig,fpigs80, na.rm=TRUE),
    fothpigs=sum(othpigs,fpigsbig,na.rm=TRUE),
    flayers=sum(layers,broilers, na.rm=TRUE),  # note didn't make fothpoul=othpoul+flayers because the SAS code didn't make sense to me and it's not in the tables anyway
    fcala=sum(cala,peas, na.rm=TRUE),
    fturswe=sum(turswe,fcala, na.rm=TRUE),
    fcarrots=sum(carrots,fturswe, na.rm=TRUE),
    #fveg=sum(veg,fcarrots, na.rm=TRUE),
    fothcer=sum(othcer, item27710, na.rm=TRUE),
    fal5=sum(item2469,item2470, na.rm=TRUE),
    othcrops=sum(item34,item2059, na.rm=TRUE),
    othcrops2=sum(item38, na.rm=TRUE),  # in SAS this includes item2319 but it's not on the dataset
    othcrops3=sum(item38,-sum(item87,item2556,item2557,item2836,item6000,item2036, na.rm=TRUE), na.rm=TRUE),
    fodder=sum(item34,item2059,item2034,item29,item30,item31,item32, na.rm=TRUE),
    othveg=sum(item55,item2323,item59,item64,item65,item66, na.rm=TRUE),
    bigpigs=sum(item152,item153, na.rm=TRUE),
    layers=sum(item158,item159, na.rm=TRUE),
    breeders=sum(item160,item162, na.rm=TRUE),
    breefowl=sum(item160,item162,item163, na.rm=TRUE),
    eggfowl=sum(item158,item159,item161, na.rm=TRUE),
    othpoul=sum(item2038,item2039,item167, na.rm=TRUE),
    breepigs=sum(item146,item147,item148, na.rm=TRUE),
    othpigs=sum(item27760, item27765, item27770, na.rm=TRUE), # why does SAS code assign othpigs twice, using different items...
    goats=item27780,
    grass=sum(item2321,item2322, na.rm=TRUE),
    potatoes=sum(item24,item2320, na.rm=TRUE),
    toats=item27715,
    tglasshouse = sum(item85,item86, na.rm=TRUE),
    cereal=sum(item14,item15,item16, item18, item17, item20, item3156, na.rm=TRUE),
    osr=item27720,
    oilseeds=sum(item27720, item21, na.rm=TRUE),
    combine=sum(item14,item15,item27710, item27715, item27720,
                item27,item28,item21, item3156, na.rm=TRUE),
    horses=item27775,
    othsheep=sum(item141,item143, na.rm=TRUE),
    ftmale=sum(item1715,item1716,item1714, na.rm=TRUE),
    ftfemale=sum(item192,item193,item1717, na.rm=TRUE),
    ptmale=sum(item194,item195,item1718, na.rm=TRUE),
    ptfemale=sum(item196,item197,item1719, na.rm=TRUE),
    ftregular=sum(ftfemale,ftmale, na.rm=TRUE),
    ptregular=sum(ptfemale,ptmale, na.rm=TRUE),
    regstaff=sum(item1715,item1716,item1714,item192,item193,item1717
                 ,item194,item195,item1718,item196,item197,item1719, na.rm=TRUE),
    casual=sum(item198,item199, na.rm=TRUE),
    occ=sum(item177,item178,item179, na.rm=TRUE),
    sp=sum(item182,item183,item184, na.rm=TRUE),
    occsp=sum(item177,item178,item179,item182,item183,item184, na.rm=TRUE),
    workforce=sum(item200,occ, sp, na.rm=TRUE),
    camelids=sum(item2472, item2473, item2474, na.rm=TRUE),
    bcat=sum(cts303,cts305,cts307, na.rm=TRUE),
    dcat=sum(cts304,cts306,cts308, na.rm=TRUE),
    mcat=sum(cts310,cts311, na.rm=TRUE),
    calves=sum(cts301,cts302,cts309, na.rm=TRUE)
  )







land_use_items<-c(land_items,all_crops, "cga", "item24","item2320","item41","item38","item39", "item2469","item2470","item40",all_grass,"item46",all_other_land,
                  "item70", "item71", "item72", "item2832","item76","item85","item86","item2556","item2557","item2836","item6000","item2036","item2037","item1711","item1943","item6001")
area_workforce_items<-c("item20026","item11","item12","item50",workforce,other_labour,"item177","item178", "item179", "item2566","item182","item183","item184", "item2567")

livestock_all_items<-c(all_cattle, all_sheep,all_pig, "item152","item153","item154","item155", all_poultry,all_other_livestock, "item27775","item27780")

added_items<-c("item87","item39","item6000","item2036","item2037", "item1711", "item1943", "lambs", "ewes",
               "pigs20","pigs50",   "pigs80",   "tpigsbig", "pigsbig",  "tothpigs", "othpigs",  "broilers", "tlayers", "layers", 
               "peas", "cala",     "turswe", "carrots",  "tothcer",  "othcer","otherfr_open","otherfr_covered",
               "strawb","raspb","blackc","blueb","tomato","otherfr","fruit","fewes","fpigs50","fpigs80","fpigsbig","fothpigs",
               "fothcer","fal5" ,"othcrops","othcrops2","othcrops3","fodder","othveg", "bigpigs","breeders", "breefowl","eggfowl", 
               "othpoul","breepigs","goats","grass","potatoes","toats","tglasshouse","cereal","osr","oilseeds","combine","horses",
               "othsheep","ftmale","ftfemale","ptmale","ptfemale","ftregular","ptregular","regstaff","casual","occ","sp",
               "occsp","workforce","camelids","bcat" ,"dcat","mcat","calves")

# check new items make sense

checkaddeditems<-jac_disag %>% 
  select(parish, holding, added_items)


# free unused R memory
gc()


# make item totals

# make sure it's ungrouped

jac_disag<-ungroup(jac_disag)

str(jac_disag)



jac_totals<-jac_disag %>% 
  dplyr::mutate(across(all_of(c(land_use_items,area_workforce_items,livestock_all_items, added_items)), ~sum(., na.rm=TRUE), .names="total{.col}", .keep="unused"))

jac_totals<-jac_totals %>% 
  dplyr::rename_with(~str_remove(.,"item"),starts_with("total")) %>% 
  dplyr::rename_with(~str_remove(.,"cts"),starts_with("total")) 


# check totals make sense

checktotals<-jac_totals %>% 
  select(starts_with("total"))


test<-jac_totals %>% 
  filter(completedata==1) %>% # change this to same logic below?
  select(id, item20026, item11) %>% 
  mutate(total20026= sum(item20026, na.rm=TRUE),
         total11=sum(item11, na.rm=TRUE))
         
jac_totals<-rows_update(jac_totals, test, by="id")                      

# SAS code also includes src = sum(total2471), item2471 not on dataset and don't know what that is. 
# Glasshouse items – some items are not on SAF 
# Didn’t make total of item2319 aromatic plants because it’s not on the dataset

# Make dataframe corresponding to tables

tables_nice<-jac_totals %>% 
  select(starts_with("total")) %>% 
  filter(row_number()==1) %>% 
  mutate(
    wheat=total14,
    triticale=total15,
    winterbarley=total16, 
    springbarley=total18,
    winteroats=total17,
    springoats=total20,
    rye=total3156,
    cereal=totalcereal,
    winterosr=total19,
    springosr=total23,
    linseed=total21,
    oilseeds=totaloilseeds,
    proteinpeas=total28,
    fieldbeans=total27,
    combine=totalcombine,
    seedpotato=total24,
    warepotato=total2320,
    allpotato=totalpotatoes,
    turnipswede=total29,
    kalecabbage=total30,
    maize=total2059,
    rape=total31,
    fodderbeet=total32,
    lupins=total2034,
    othercropsstockfeed=total34,
    fodder=totalfodder,
    veghuman=total27730,
    orchardfruit=total36,
    softfruit=totalfruit,
    othercrops=totalothcrops3, 
    fallowunder5=total2469,
    fallowover5=total2470,
    allfallow=total39,
    cropsfallowsetaside=total40,
    grassunder5=total2321,
    grassover5=total2322,
    allgrass=totalgrass,
    cropsgrass=total46,
    roughgrazing=total47,
    otherincwoodland=total49+total48,
    agriculturalarea=total50,
    commongrazing=totalcga,  # this isn't in SAS so I made it using the only common grazing variable
    peascanning=total52,
    beanscanning=total53,
    turnipswedehuman=total56,
    calabrese=total60,
    cauliflowerbroccoli=total61,
    carrots=total63,
    otherveg=totalothveg,
    bulbsflowers=total27740,
    strawberries=total70,
    raspberries=total71,
    blueberries=total2832,
    otherfruitopen=totalotherfr_open,
    totalsoftfruitopen=total76,
    tomatoesundercover=total87,
    strawbundercover=total2556,
    raspbundercover=total2557,
    bluebundercover=total2836,
    otherfruitundercover=totalotherfr_covered,
    vegundercover=total2037,
    beddingpotundercover=total1711,
    hardyundercover=total1943,
    unusedundercover=total6001,
    alltomato=totaltomato,
    allstrawb=totalstrawb,
    allraspb=totalraspb,
    allblackc=totalblackc,
    allblueb=totalblueb,
    allotherfruit=totalotherfr,
    allsoftfruit=totalfruit,
    plastic=total85,
    glass=total86,
    allglasshouse=totaltglasshouse,
    femaledairy1to2=total304,
    femaledairy2off=total306,
    femaledairy2nooff=total308,
    alldairy=totaldcat,
    femalebeef1to2=total303,
    femalebeef2off=total305,
    femalebee2nooff=total307,
    allbeef=totalbcat,
    male1to2=total310,
    male2=total311,
    allmale=totalmcat,
    femaledairyunder1=total302,
    femalebeefunder1=total301,
    maleunder1=total309,
    allcalves=totalcalves,
    allcattle=total312,
    ewes=total139,
    rams=total140,
    othersheep1=total141,
    othersheep=total143,
    allothersheep=totalothsheep,
    lambs=total144,
    allsheep=total145,
    sowsinpig=total146,
    gltsinpig=total147,
    othersows=total148,
    allbreedingpigs=totalbreepigs,
    barrensows=total149,
    gilts50=total150,
    boars=total151,
    pigs80=total152+total153,
    pigs50to80=total154,
    pigsunder50=total155+total27765+total27770,
    allotherpigs=pigs80+pigs50to80+pigsunder50,
    allpigs=total157,
    pulletshenslaying=totallayers,
    pulletsrearedlaying=total161, 
    allfowlseggs=totaleggfowl,
    breedinghens=totalbreeders,
    cocks=total163,
    allbreedingfowl=totalbreefowl,
    broilers=total164,
    turkeys=total1708,
    otherpoultry=totalothpoul,
    allpoultry=total170,
    deer=total94, 
    horses=totalhorses,
    donkeys=total2868,
    goats=totalgoats,
    camelids=totalcamelids,
    beehives=total2826,
    occ1occ2ft=total177+total182,
    occ1occ2ht=total178+total183,
    occ1occ2lht=total179+total184,
    allwork=totaloccsp,
    nowork=total2566+total2567,
    ftmaleshired=total1715,
    ftmalesfamily=total1716,
    ftmalespartners=total1714,
    allftmales=totalftmale,
    ftfemaleshired=total192,
    ftfemalesfamily=total193,
    ftfemalespartners=total1717,
    allftfemales=totalftfemale,
    allft=totalftregular,
    ptmaleshired=total194,
    ptmalesfamily=total195, 
    ptmalespartners=total1718,
    allptmales=totalptmale,
    ptfemaleshired=total196,
    ptfemalesfamily=total197, 
    ptfemalespartners=total1719,
    allptfemales=totalptfemale,
    allpt=totalptregular,
    allregstaff=totalregstaff,
    malescasual=total198,
    femalescasual=total199,
    allcasual=totalcasual,
    workforce=totalworkforce,
    european=total2536,
    noneuropean=total2511,
    rented=total20026,
    owned=total11,
    areaoccupied=total20026+total11
    
    
  ) %>% 
  select(-starts_with("total"))


checkholdings<-jac %>% 
  select(id, survtype, madeup, completedata, land_data, item20026, item11, item50) %>% 
  filter(!(completedata==0&(survtype=="CTS only"|survtype=="C/Grazings"|(survtype=="none"&madeup>10))))

holdingscomplete<-jac %>% 
  filter(completedata==1) %>% 
  select(id,  survtype, madeup, ags_madeup, saf_madeup, completedata, land_data, item12, item50, item46, item145, item157, item170)


#  only keep 2021 holdings which are in total jac 2023


jac2021$id<-paste0(jac2021$parish,"_",jac2021$holding)

jac2021red<-jac2021[jac2021$id %in% jac$id,]

holdingscomplete2021<-jac2021red %>% 
  filter(completedata==1)

holdingscomplete2021<-holdingscomplete2021 %>% 
  select(id,  survtype, madeup, ags_madeup, saf_madeup, completedata, land_data, item12, item50, item46, item145, item157, item170)

missingholdings<-holdingscomplete2021[!holdingscomplete2021$id %in% holdingscomplete$id,]

poultryholdings<-missingholdings %>% 
  filter(item170>0) %>% 
  filter(!madeup>=10)

excluded<-jac %>% 
  filter(completedata==0) %>% 
  select(id,  survtype, madeup, ags_madeup, saf_madeup, completedata, land_data, item12, item50, item46, item145, item157, item170)


cropsholdings<-missingholdings %>% 
  filter(item46>0)

cropsholdings2023<-jac[jac$id %in% cropsholdings$id,]

cropsholdings2023<-cropsholdings2023 %>% 
  select(id,  survtype, madeup, ags_madeup, saf_madeup, completedata, land_data, item12, item50, item46, item145, item157, item170)


missingholdings<-holdingscomplete[!holdingscomplete$id %in% holdingscomplete2021$id,]



# 
# checkholdings<-jac %>% 
#   select(id, survtype, madeup, completedata, land_data, imptype, item50, item46, item47, item48, item49, item40, item2321, item2322, item35, item37, item38, all_crops, item41, item84, item85, item86, all_glasshouse, item68, item76, all_veg, all_fruit) %>% 
#   filter(item50==0|is.na(item50))

# correct pigs in the meantime before re-run of disaggregation etc

# jac_totals <- jac_totals %>% 
# mutate(pigs80=67620,
#        pigs50to80=47721,
#        pigsunder50=187086,
#        allotherpigs=302427)

tables<- tables_nice %>% pivot_longer(everything(), names_to = "categories", values_to = "number")



# Save --------------------------------------------------------------------


write.csv(tables,"//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Main/June23/tables_18.10.23.csv" )

# Save data

write_dataframe_to_db(
  server = server,
  database = database,
  schema = schema,
  table_name = "JAC23_final_dataset_totals_18_10",
  dataframe = jac_totals,
  append_to_existing = FALSE,
  versioned_table = FALSE,
  batch_size = 10000
)



# Checks ------------------------------------------------------------------

# 
# checkpigs<-jac_totals %>% 
#   select(parish, holding, item27760, item152, item153, item154, item155)
# 
# checkbroiler<-jac_totals %>% 
#   select(parish, holding, item164) %>% 
#   select(parish, holding, item164) %>% 
#   mutate(totalbroiler=sum(item164, na.rm=TRUE))
# 
# 
# checkbroiler2021<-previous_years %>% 
#   filter(yr==2021 & ags_madeup==0) %>% 
#   select(parish, holding, item164, ags_madeup) %>% 
#   mutate(totalbroiler=sum(item164, na.rm=TRUE))
# 
# 
# checkbroiler2021<-previous_years %>% 
#   filter(yr==2021 & ags_madeup>0) %>% 
#   select(parish, holding, item164, ags_madeup) %>% 
#   mutate(totalbroiler=sum(item164, na.rm=TRUE))
# 
# 
# checkoccsp<-jac_totals %>% 
#   select(parish, holding, land_data, item177, item178, item179, item182, item183, item184, item194, item195)
# 
# checkoccspextract<-df_nonSAF %>% 
#   select(parish, holding, item177, item178, item179, item182, item183, item184, item194, item195)
# 
# checkcg<-previous_years %>% 
#   filter(yr==2021) %>% 
#   select(parish, holding, land_data, cgarea)
# 
# checkoccsprealprev<-previous_years %>% 
#   filter(yr==2021) %>% 
#   select(parish, holding, ags_madeup, survtype, item177, item178, item179, item182, item183, item184, item2566, item2567) %>% 
#   filter(ags_madeup==0) %>% 
#   dplyr::mutate(across(any_of(occandocc2), ~ifelse (.>0, 1, .))) %>% 
#   rowwise() %>% 
#   mutate(occsp=sum(item177, item178, item179, item182, item183, item184, na.rm=TRUE)) %>% 
#   mutate(nowork=sum(item2566, item2567, na.rm=TRUE))
# 
# sum(checkoccsprealprev$occsp, na.rm=TRUE)
# sum(checkoccsprealprev$nowork, na.rm=TRUE)
# 
# 
# 
# checkoccsprfprev<-previous_years %>% 
#   filter(yr==2021) %>% 
#   select(parish, holding, ags_madeup, survtype, item177, item178, item179, item182, item183, item184, item2566, item2567) %>% 
#   filter(ags_madeup>0) %>% 
#   dplyr::mutate(across(any_of(occandocc2), ~ifelse (.>0, 1, .))) %>% 
#   rowwise() %>% 
#   mutate(occsp=sum(item177, item178, item179, item182, item183, item184, na.rm=TRUE)) %>% 
#   mutate(nowork=sum(item2566, item2567, na.rm=TRUE))
# 
# sum(checkoccsprfprev$occsp, na.rm=TRUE)
# sum(checkoccsprfprev$nowork, na.rm=TRUE)
# 
# 
# checkoccspreal<-jac_disag %>% 
#   filter(survtype=="Non-SAF") %>% 
#   select(parish, holding, ags_madeup, survtype, item177, item178, item179, item182, item183, item184, item2566, item2567) %>% 
#   rowwise() %>% 
#   mutate(occsp=sum(item177, item178, item179, item182, item183, item184, na.rm=TRUE)) %>% 
#   mutate(nowork=sum(item2566, item2567, na.rm=TRUE))
# 
# sum(checkoccspreal$occsp, na.rm=TRUE)
# sum(checkoccspreal$nowork, na.rm=TRUE)
# 
# 
# checkoccsprf<-jac_disag %>% 
#   filter(survtype!="Non-SAF") %>% 
#   select(parish, holding, ags_madeup, survtype, item177, item178, item179, item182, item183, item184, item2566, item2567) %>% 
#   rowwise() %>% 
#   mutate(occsp=sum(item177, item178, item179, item182, item183, item184, na.rm=TRUE)) %>% 
#   mutate(nowork=sum(item2566, item2567, na.rm=TRUE))
# 
# sum(checkoccsprf$occsp, na.rm=TRUE)
# sum(checkoccsprf$nowork, na.rm=TRUE)
# 
# 
# 
# # check migrant labour
# 
# 
# migrants<-jac_totals %>% 
#   select(parish, holding, survtype, land_data, imptype, item2511, item2536)
# 
# item50final<-jac_totals %>% 
#   select(parish, holding, survtype, land_data, imptype, item46, item50)
# 
# item50<-whole_population %>% 
#   select(parish, holding, survtype, land_data, imptype, item46, item47, item48, item49, item50)
# 
# 
# holdingsitem50<-df_nonSAF %>% 
#   filter((parish==94&holding==49)|(parish==662&holding==44)|(parish==673&holding==10)|(parish==669&holding==4)|(parish==674&holding==44))
# # 
# # 
# # # check workforce
# # 
# workers<-jac_totals %>%
#   select(parish, holding,imptype,submis_type, imptype,"item200", "item1714", "item1715", "item1716", "item1717", "item192", "item193", "item1718", "item194", "item195", "item1719", "item196", "item197", "item198", "item199", "item200")
# 
# 
# sum(workers$item1717, na.rm=TRUE)
# 
# checkturkeys<-jac_totals %>% 
#   select(parish, holding, imptype, item1708, item2038, item2039, item167)
# 
# 
# checkturkeysprev<-previous_years %>% 
#   filter(yr==2021 &ags_madeup==0) %>% 
#   select(parish, holding, imptype, item1708, item2038, item2039, item167)
# 
# sum(checkturkeysprev$item1708, na.rm=TRUE)
# 
# checkitem40<-jac_disag %>% 
#   select(parish, holding, item40,item14, item15, item16, item18, item17, item20, item3156, item19, 
#          item23, item21, item24, item2320, item27, item28, item2034, item29, item30,
#          item31, item2059, item32, item34, item36, item2469, item2470, item35, item37, item38)


# Fertiliser use

# all_ags <- read_table_from_db(server=server, 
#                               database=database, 
#                               schema=schema, 
#                               table_name="Ags_A_2023")
# 
# 
# fertiliserna<-all_ags %>% 
#   select(parish, holding, item5100, item5101) %>% 
#   filter((is.na(item5100)& is.na(item5101)))
# 
# 
# fertiliserinczero<-all_ags %>% 
#   select(parish, holding, item5100, item5101) %>% 
#   filter(!(is.na(item5100)& is.na(item5101)))
# 
# fertiliserpositive<-all_ags %>% 
#   select(parish, holding, item5100, item5101) %>% 
#   filter(item5100>0 |item5101>0)
# 
# 
# rotationna<-all_ags %>% 
#   select(parish, holding, item2653, item2654) %>% 
#   filter((is.na(item2653)& is.na(item2654)))
# 
# 
# rotationinczero<-all_ags %>% 
#   select(parish, holding, item2653, item2654) %>% 
#   filter(!(is.na(item2653)& is.na(item2654)))
# 
# rotationpositive<-all_ags %>% 
#   select(parish, holding, item2653, item2654) %>% 
#   filter(item2653>0 |item2654>0)
# 
# 
# somerotation<-jac %>% 
#   select(parish, holding, item2653, item2654, completedata) %>% 
#   filter(item2653>0)

