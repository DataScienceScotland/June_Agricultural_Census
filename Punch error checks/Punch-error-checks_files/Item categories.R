
#June Item categories----------------------------------------------------------------------------------------------------------------------------------------------------
all_holding_areas <- c("item7", "item11", "item12", "item125", "item126", "item127", "item2249")
all_seasonal <- c("item1", "item2827", "item2828")
all_seasonal_saf <- c("item1", "item2879")
all_glasshouse_ags <- c("item2713", "item2707", "item2866", "item2714", "item2708", "item2715", "item2709", "item2716", "item2710", "item2717", "item2711", "item2862", "item2867")
all_glasshouse_saf <- c("item2858", "item2859", "item2860", "item2861", "item2863", "item2864", "item2865")
all_glasshouse <- c(all_glasshouse_ags, all_glasshouse_saf, "item85", "item86")
all_cattle <- c("cts301", "cts302", "cts303", "cts304", "cts305", "cts306", "cts307", "cts308", "cts309", "cts310", "cts311", "cts312")
all_sheep <- c("item139", "item140", "item141", "item144", "item145")
all_crops <- c("item14", "item15", "item16", "item18", "item17", "item20", "item3156", "item22", "item19", "item23", "item21", "item24", "item2320", "item27", "item28", "item2034", "item29", "item30", "item31", "item2059", "item32", "item34", "item36", "item2469", "item2470")
all_grass <- c("item2321", "item2322")
all_other_land <- c("item47", "item48", "item49")
all_veg  <- c("item52", "item53", "item55", "item56", "item2323", "item59", "item60", "item61", "item63", "item64", "item65", "item66")
all_fruit <- c("item70", "item71", "item72", "item2832", "item75")
all_nursery <- c("item2324", "item1709", "item80", "item81", "item1710", "item82", "item83")
#to change in JAC 2023, less items for pigs
all_pig <-c("item146", "item147", "item148", "item149", "item50", "item151", "item152", "item153", "item154", "item155", "item156", "item157")
all_poultry <- c("item158", "item159", "item160", "item161", "item162", "item163", "item164", "item167", "item170", "item1708", "item2038", "item2039")
all_other_livestock <- c("item94", "item95", "item96", "item1712", "item1713", "item98", "item2472", "item2473", "item2474", "item2826", "item2868", "item171")
#to change for JAC 2023? Occupier 1/2 
occupier_12 <- c("item177", "item178", "item179", "item2566", "item182", "item183", "item184", "item2567", "item2877", "item2878", "item3056", "item3057")
workforce <- c("item1714", "item1715", "item1716", "item1717", "item192", "item193", "item1718", "item194", "item195", "item1719", "item196", "item197", "item198", "item199", "item200")
other_labour <- c("item2712", "item2066", "item2511")
ags_leftover <- c("item48", "item2980")
string_answers <- c("item185", "item186")



#all June Census items-----------------------------------------------------------------------------------------------------------------------
all_items <- c(all_holding_areas, all_seasonal, all_seasonal_saf, 
               "item2", all_crops, "item35", "item37", "item38", "item40", "item41", 
               all_grass, "item46", all_other_land, "item50", all_veg, "item68",
               all_fruit, "item76", all_nursery, "item84", all_glasshouse, "item85", "item86", "item2836",
               all_cattle, "cts312", all_sheep, "item145", all_pig, "item157", all_poultry, "item170",
               all_other_livestock, "item171", occupier_12, workforce, "item200", other_labour, "item2980") 


#this year's June items for punch error checks------------------------------------------------------------------------------------------------------

punch_June_items <- c("item7", "item11", "item12", "item40", "item157", "item170", "item2321", "item145", "item139", "item140", "item141", "item143", "item144", "item47", "item49", "item2322", "item37", "item2469", "item2470", "item85", "item86", "item24", "item2320", "item160", "item162", "item163", "item94", "item50", "item2511")

#prev years items seem to be the same as "this year's"

punch_June_items_yr1 <- punch_June_items

punch_June_items_yr2 <- punch_June_items

perr1_vars<-c("perr1", "item12", "oitem12", "voitem12", "grass", "ograss", "vograss", "othgrass", "oothgrass", "voothgrass", "item47", "oitem47", "voitem47", "tillage", "otillage", "votillage") 
perr2_vars <- c("perr2", "item157", "oitem157", "voitem157")
perr3_vars <- c("perr3", "item170", "oitem170", "voitem170")
perr4_vars <- c("perr4", "cts312", "octs312", "vocts312")
perr5_vars <- c("perr5", "dairy", "odairy", "vodairy")
perr6_vars <- c("perr6","beef", "obeef", "vobeef")
perr7_vars <- c("perr7", "item145", "oitem145", "voitem145")
perr9_vars <- c("perr9",  "item37", "oitem37", "voitem37")
perr10_vars <- c("perr10", "glass", "oglass", "voglass")
perr11_vars <- c("perr11", "male", "omale", "vomale")
perr12_vars <- c("perr12", "female", "ofemale", "vofemale")
perr13_vars <- c("perr13", "ware", "oware", "voware")
perr14_vars <- c("perr14", "item94", "oitem94", "voitem94")
perr15_vars <- c("perr15", "item50", "oitem50", "voitem50")
perr16_vars <- c("perr16", "item47", "oitem47", "voitem47")
perr17_vars <- c("perr17", "item2511", "oitem2511", "voitem2511")


