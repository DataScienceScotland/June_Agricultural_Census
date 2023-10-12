# Before import -----------------------------------------------------------


# Clear environment prior

rm(list = ls())

#yr = this year 20xx
yr <- 23
yr1 <- yr - 2
yr2 <-  yr - 3
#yr3 <-  yr - 4
# Load packages
library(plyr)
library(tidyverse)
library(RtoSQLServer)
library(readxl)




#<-Datashare<-file path for import and export
Code_directory<-("//s0177a/datashare/seerad/ags/census/branch1/NewStructure/Surveys/June/Codeconversion_2023/2023")
sas_agstemp_path <-   "//s0177a/sasdata1/ags/census/agstemp/"
sas_agscens_path <-   "//s0177a/sasdata1/ags/census/agscens/"

# ADM schema for export

server <-   "s0196a\\ADM"
database <-   "RuralAndEnvironmentalScienceFarmingStatistics"
schema <-   "juneagriculturalsurvey2023alpha"



#Import-------------------------------------------------------------

june_temp <-  read_table_from_db(server=server, 
                                 database=database, 
                                 schema=schema, 
                                 table_name="JAC23_temp")

# #prev yr 
# june_21 <- read_table_from_db(
#     server = server,
#     database = database,
#     schema = schema,
#     table_name = "jac_2021_update"
#   )


#address file for LFAs (2023)
address_2023 <-  read_table_from_db(server=server, 
                                       database=database, 
                                       schema=schema, 
                                       table_name="address_occid_01jun_2023")

#address files for poultry classification (prev 3 years)
address_2021 <-  read_table_from_db(server=server, 
                                    database=database, 
                                    schema=schema, 
                                    table_name="address_occid_01jun21")

address_2020 <-  read_table_from_db(server=server, 
                                    database=database, 
                                    schema=schema, 
                                    table_name="address_occid_01jun20")


#address email for BRN
brn <-  read_table_from_db(server=server, 
                                    database=database, 
                                    schema=schema, 
                                    table_name="address_email_24may22")


#formats
agscens_format <-read_table_from_db(server = server,
                                    database = database,
                                    schema = schema,
                                    table_name = "agscens_SAS_format")


# Add Standard Outputs ----------------------------------------------------

june_so <- june_temp %>% filter(completedata==1) %>% 
  dplyr::mutate(sd01   = 1411.04,
           sd03   = 918.57,
           sd04  = 964.51,     
           sd05  = 953.72,    
           sd08   = 849.24,    
           sd09   = 923.59,    
           sd10   = 6876.9,   
           sd12   = 225.42,   
           sd14a   = 2125.34,
           sd14b	= 11241.4, 
           sd15    = 128344.6,
           sd16    = 33314.26, 
           sd17    = 292875.61,
           sd18a	= 331.4,
           sd18b1	= 412.2,
           sd18b2_3 = 225.42, 
           sd19    = 1796.75,  
           sd20    = 626.15,   
           sd21	= 1,  
           sd22   = 0,     
           sd26   = 1083.41,
           sd29   = 606.57,   
           sf01	= 299.27,
           sf02	= 1,  
           sf03	= 0, 
           sg01    = 7974.33,  
           sg05	= 119456.1, 
           #livestock
           sj01    = 298,
           sj02    = 529.52,
           sj03    = 607.3,
           sj04    = 400, 
           sj05    = 537.6, 
           sj06    = 380.27, 
           sj07    = 1698.54, 
           sj08   = 462.42,   
           sj09a  = 55.31,    
           sj09b = 23.84,	  
           sj10a   = 344.67,    
           sj10b  = 47.47,   
           sj11   = 86.02,	   
           sj12   = 612.93,   
           sj13   = 237.46,   
           sj14   = 10.6457,  
           sj15   = 17.8248,  
           sj16a 	= 96.6076, 
           sj16b 	= 42.6608, 
           sj16c	= 54.9,
           sj16d 	= 15.4672,
  )


# Calculate SGM -----------------------------------------------------------

june_so <- june_so %>% rowwise() %>% 
  dplyr::mutate(a05 = case_when(lfatemp ==1| lfatemp==2 ~ 1,
                         TRUE ~ 0),
         d01 = item14,
        d03 = item3156,
        d04 = sum(item16,item18, na.rm=TRUE),
        d05 = sum(item17,
                  item20,
                  #item22,
                  na.rm=TRUE),
 
        d08 = item15,
        d09 = sum(item27,item28,item2034, na.rm=TRUE),
        d10 = sum(item24, item2320, na.rm=TRUE),
        d12 = sum(item29,item32, na.rm=TRUE),
        d26 = sum(item19,item23, na.rm=TRUE),
        d29 = item21,
        tot = sum(item55,
               item56,
               item59,
               item60, 
               item61,
               item63,
               item64, 
               item65, 
               item66, 
               item70,
               item2323, na.rm = TRUE)
        )


  

june_so<- june_so %>% dplyr::mutate(subtotal = rowSums(across(c(item55, 
                        item56,
                        item59,
                        item60,
                        item61,
                        item63,
                        item64, 
                        item65, 
                        item66, 
                        item70,
                        item2323), ~ replace(.x, .x<2, 0)), na.rm = TRUE))
  

#replace prev items with june 2023 equivalents (previous items are commented out)

june_so <- june_so %>%rowwise %>% 
  dplyr::mutate(d14a = case_when(tot>0 ~case_when(3*subtotal>2*tot ~ sum(item52,
                                                         item53,
                                                         item55, 
                                                         item56, 
                                                         item59, 
                                                         item60,
                                                         item61,
                                                         item63,
                                                         item64,
                                                         item65, 
                                                         item70, 
                                                         item2323, na.rm =TRUE),
                                           TRUE~sum(item52,
                                                    item53, na.rm = TRUE)),
                          TRUE ~  sum(item52,
                                      item53, na.rm=TRUE)),
         d14b = case_when(tot>0 ~case_when(3*subtotal>2*tot ~ item66,
                                           TRUE~sum(item55, 
                                                    item56, 
                                                    item59, 
                                                    item60,
                                                    item61,
                                                    item63,
                                                    item64,
                                                    item65, 
                                                    item70, 
                                                    item2323, na.rm =TRUE)),
                          TRUE ~item66),
         threshold = 1,
         # d15 = sum(#item87,
         #           #item2036,
         #           #item2037, 
         #           item2556,
         #           item2557, 
         #           item2836, 
         #           #item6000, 
         #           na.rm = TRUE),
         d15 = sum(#item2713,
                   item2707,
          #item2714, 
          #item2708,
          #item2715,
          #item2709,
           item2556,
           item2557, 
           item2836, 
          #item2866,
          item2861,
           na.rm = TRUE),
        d16 = sum(item2324,
                  item1709,
                  item81,
                  item82,
                  item1710,
                  item83, na.rm=TRUE),
        # d17 = case_when(sum(#item1711,
        #                     #item1943,
        #                     #na.rm =TRUE)<0 ~ 0,
        #                 TRUE ~ sum(#item1711,
        #                            item1943, 
        #                            na.rm =TRUE)),
        # d17 = case_when(sum(#item2716, 
        #                     #item2710,
        #                     #item2717,
        #                     #item2711,
        #                     #na.rm =TRUE)<0 ~ 0,
        #                 TRUE ~ sum(item2716, 
        #                            item2710,
        #                            item2717,
        #                            item2711,
        #                            na.rm =TRUE)),
        d17 =0,
        d18a = item2321,
        d18b1 = item2059,
        d18b2_3 = sum(item30,
                      item31,
                      item34, na.rm=TRUE),
        d20=item41,
        d21=item2469,
        
        #SAS:
        #code overwrites prev tot = sum(market_veg) to tot = item50... haven't done this here                                      
        
        f01 = item2322,
        f02 = item47, 
        f03 = item2470,
        f00 = sum(f01,
                  f02,
                  f03, na.rm=TRUE),
        g01a = sum(item36,
                   item71,
                   item72,
                   item2832,
                   item75, na.rm=TRUE),
        g05a = item80,
        # j01 = sum(#item95,
        #           #item96, 
        #           item2868, na.rm=TRUE), 
        j01 = sum(item27775,
                     item2868, na.rm=TRUE), 
        j02 = sum(CTS301, 
                  CTS302,
                  CTS309, na.rm=TRUE),
        j03 = CTS310,
        j04 = sum(CTS303,
                  CTS304, na.rm=TRUE),
        j05 = CTS311,
        j06 = sum(CTS307, 
                  CTS308, na.rm=TRUE),
        j07 = CTS306, 
        j08 = CTS305,
        j09a= item139,
        j09b= sum(item140,
                  item141,
                  item143,
                  item144, na.rm=TRUE),
        # j10a = sum(item1712,
        #            item1713, na.rm=TRUE),
        j10a= item27780,
        #j10b = item98
        j10b = 0,
        #j11 = item156,
        j11= item27770,
        j12 = sum(item146,
                  item147,
                  item148,
                  item150, na.rm=TRUE),
        # j13 = sum(item149,
        #           item151,
        #           #item152,
        #           #item153,
        #           #item154,
        #           #item155, 
        #           na.rm=TRUE),
        j13 = sum(item149,
                  item151,
                  item27760,
                  item27765,
                  na.rm=TRUE),
        j14 = item164,
        j15 = sum(item158,
                  item159,
                  item160,
                  item161,
                  item162,
                  item163, na.rm=TRUE),
        j16 = sum(item167,
                  item1708,
                  item2038,
                  item2039, na.rm=TRUE),
        j16a = item1708,
        j16b = item2038,
        j16c = item2039,
        j16d = item167, 
        j18 = item2826,
        j19i = item94,
        j19ii = sum(item2472, 
                    item2473, 
                    item2474, na.rm=TRUE) )
        

# Calculate SO 2013 -------------------------------------------------------

        
                        
 
         
june_so <- june_so %>%rowwise %>% 
  dplyr::mutate( 
    d01a 	= d01*sd01,
      d03a 	= d03 * sd03,
      d04a 	= d04  * sd04,
      d05a 	= d05  * sd05,
      d08a 	= d08 * sd08,
      d09a 	= d09 * sd09,
      d10a 	= d10 * sd10, 
      d12a  	= d12 * sd12,
      d14aa 	= d14a * sd14a,
      d14ba 	= d14b * sd14b,
      d15a 	= d15  * sd15,
      d16a 	= d16  * sd16, 
      d17a 	= d17  * sd17,
      d18ax 	= d18a * sd18a,
      d18b1x 	= d18b1 * sd18b1, 
      d18b2_3x = d18b2_3 * sd18b2_3,
      d18bx 	= sum(d18b1x, d18b2_3x, na.rm= TRUE),
      d18tot 	= sum(d18ax, d18bx, na.rm=TRUE),
      d20a 	= d20  * sd20, 
      d26a	= d26 * sd26, 
      d29a	= d29 * sd29,				
      f01a	= f01 * sf01,			
      f02a	= f02 * sf02,			
      f03a	= f03 * sf03,				
      g01aa 	= g01a * sg01,			
      g05aa 	= g05a * sg05,				
  )
         
#livestock

june_so <- june_so %>%rowwise %>% 
  dplyr::mutate(j01a = j01 * sj01,
          j02a = case_when(j02-j07-j08>0 ~  sj02*(j02-j07-j08),
                           TRUE~0),
          j03a = j03 * sj03, 
          j04a = j04 * sj04, 
          j05a = j05 * sj05, 
          j06a = j06 * sj06,
          j07a = j07 * sj07, 
          j08a = j08 * sj08, 
          j09ax = j09a * sj09a,
          j09bx = case_when(j09a>0 ~ 0,
                            TRUE~j09b*sj09b),
          j10ax = j10a * sj10a,
          j10bx = case_when(j10a>0 ~ 0,
                            TRUE~j10b*sj10b),
          j09tot = sum(j09ax, j09bx, na.rm=TRUE),
          j10tot = sum(j10ax, j10bx, na.rm=TRUE),
          j12a = j12 * sj12,
          j11a = case_when(j12a>0 ~ 0,
                           TRUE~ j11 * sj11),
          j13a = j13 * sj13,
          j14a = j14 * sj14,
          j15a = j15 * sj15, 
          j16a = sum(sum(j16a * sj16a, na.rm= TRUE),
                     sum(j16b * sj16b, na.rm= TRUE), 
                     sum(j16c * sj16c, na.rm = TRUE), 
                     sum(j16d * sj16d, na.rm = TRUE),
                     na.rm=TRUE) 
          #j16a = (j16a * sj16a) + (j16b * sj16b) + (j16c * sj16c) + (j16d * sj16d),
          
            
          )
#cattle

june_so <- june_so %>%rowwise %>% 
  dplyr::mutate(P45 = sum(j02a,
                   j04a,
                   j06a, 
                   j07a, 
                   na.rm=TRUE),
         
         P46 = sum(P45,
                   j03a,
                   j05a, 
                   j08a,
                   na.rm=TRUE),
         GL = sum(P46,
                  j01a,
                  j09ax,
                  j09bx,
                  j10tot,
                  na.rm=TRUE),
         FCP1 = case_when(GL ==0 ~sum(d12a, 
                                     d18tot,
                                     f01a, 
                                     f02a, 
                                     na.rm=TRUE),
                          TRUE ~ 0),
         FCP4 = case_when(GL==0 ~ 0,
                          TRUE~sum(d12a, 
                                   d18tot,
                                   f01a, 
                                   f02a, 
                                   na.rm=TRUE)),
         P17 = case_when(GL== 0 ~ sum(d10a, 
                                     d12a,
                                     na.rm=TRUE),
                         TRUE ~ d10a),
         P151 = sum(d01a,
                    d03a, 
                    d04a,
                    d05a, 
                    d08a,
                    na.rm=TRUE),
         P15 = P151,
         P16 = sum(d26a, d29a, na.rm=TRUE),	
         P51 = sum(j11a, j12a, j13a, na.rm=TRUE),
         P52 = sum(j14a, j15a, j16a, na.rm=TRUE),	
         P1 = sum(P15, d09a, d10a, P16, d14aa, d20a, FCP1, na.rm=TRUE),
         P2 = sum(d14ba, d16a, d17a, g05aa, na.rm=TRUE),
         P3 = sum(d15a, g01aa, na.rm=TRUE),
         P4 = sum(GL, FCP4, na.rm=TRUE),
         P5 = sum(P51, P52, na.rm=TRUE),
         sumso = sum(P1, P2, P3, P4, P5, na.rm = TRUE),
         typso_2_3 = 0.667*sumso,
         typso_1_3 = 0.333*sumso,
         typgz_3_4 =  0.75*GL,
         typgz_p4  = 0.333*P4,
         typgz_p4x = 0.1*P4,
         typgz_2_3 = 0.667*GL,
         typgz_1_10 = 0.1*GL,
         typgz_1_3  = 0.333*GL
           
         )

    
    
  

# Typology Construction ---------------------------------------------------
june_so <- june_so %>%rowwise %>% 
  dplyr::mutate(typhigh = case_when(P1>typso_2_3 ~ 1,
                                    P2>typso_2_3 ~ 2,
                                    P3>typso_2_3 ~ 3,
                                    P4>typso_2_3 ~ 4,
                                    P5>typso_2_3 ~ 5,
                                    sum(P1,P2,P3, na.rm=TRUE)> typso_2_3 ~ 6,
                                    sum(P4, P5, na.rm=TRUE)>typso_2_3 ~ 7,
                                    sumso>0 ~ 8,
                                    sumso == 0 | is.na(sumso) ~ 9
                                    
  )
  )


june_so <- june_so %>%rowwise %>% 
  dplyr::mutate(typmed = case_when(typhigh==1 & sum(P15,
                                                    P16,
                                                    d09a, na.rm=TRUE)>typso_2_3 ~ 15,
                                   typhigh==1 & sum(P15,
                                                    P16,
                                                    d09a, na.rm=TRUE)<=typso_2_3 ~ 16,
                                   typhigh==2 & sum(d15a, d17a, na.rm=TRUE)> typso_2_3 ~21,
                                   typhigh==2 & sum(d14ba, d16a, na.rm=TRUE)> typso_2_3 ~22,
                                   typhigh==2 & sum(d15a, d17a, na.rm=TRUE)< typso_2_3 & sum(d14ba, d16a)< typso_2_3  ~21,
                                   typhigh==3 ~36,
                                   typhigh==4 & j07a> typgz_3_4 & GL> typgz_p4x ~45,
                                   typhigh==4 & P46> typgz_2_3 & j07a<= typgz_1_10 & GL>typgz_p4x ~46))


june_so <- june_so %>%rowwise %>% 
  dplyr::mutate(typmed = case_when(
    typhigh==4 & P46> typgz_2_3 & j07a> typgz_1_10 & GL>typgz_p4x & typmed !=45 ~ 47,
    typhigh==4 & !(typmed %in% c(45, 46, 47))~ 48,
    typhigh==5 & P51>typso_2_3 ~ 51,
    typhigh==5 & P52>typso_2_3 ~ 52,
    typhigh==5 & !(typmed %in% c(51, 52)) ~ 53,
    typhigh==6 & (P1<=typso_2_3 & P2<=typso_2_3 & P3 <= typso_2_3) ~ 61,
    typhigh==7 & (P4<=typso_2_3 & P5<=typso_2_3) & P4 > P5 ~73,
    typhigh==7 & (P4<=typso_2_3 & P5<=typso_2_3) & P4 <= P5 ~74,
    typhigh==8 & P1>typso_1_3 & P4>typso_1_3 ~ 83,
    typhigh==8 & typmed != 83~ 84,
    typhigh==9 & sumso==0 ~90,
    TRUE~as.numeric(typmed))
  )


june_so <- june_so %>%rowwise %>% 
  dplyr::mutate(sgm2000=sumso,
                EC_SGM = sumso/0.8526,
                ecsgm1 = sgm2000/1200,
                ectype1 = case_when(typmed == 15 ~	13,
                                    typmed == 16 ~ 14,
                                    typhigh == 2 ~ 20,
                                    typhigh == 3 ~ 30,
                                    typmed == 45 ~ 41,
                                    typmed == 46 ~ 42,
                                    typmed == 47 ~ 43,
                                    typmed == 48 ~ 44,
                                    typhigh == 5 ~ 50,
                                    typhigh == 6 ~ 60,
                                    typmed == 73 ~ 71,
                                    typmed == 74 ~ 72,
                                    typmed == 83 ~ 81,
                                    typmed == 84 ~ 82,
                                    TRUE~  99
                                    
                                    
                                    
                )
  )









june_so <- june_so %>%rowwise %>%
  dplyr::mutate(typlow = case_when(typmed==15 & 
                                     sum(P151,
                                         P16,
                                         d09a)>typso_2_3 ~ 151))

june_so <- june_so %>%rowwise %>%
  dplyr::mutate(typlow = case_when(                 
    typmed==15 & typlow!=151 ~ 153,
    typmed==16 & sum(P15, 
                     P16,
                     d09a, na.rm=TRUE)> typso_1_3 & P17> typso_1_3 ~162,
    typmed==16 & d14aa > typso_2_3 ~ 163,
    TRUE~ as.numeric(typlow)))

june_so <- june_so %>%rowwise %>%
  dplyr::mutate(typlow = case_when( typmed==16 & !(typlow %in% c(161, 162, 163)) ~ 166,
                                    typmed==21 & d15a> typso_2_3 ~ 211,
                                    typmed==21 &  d17a> typso_2_3 ~212,
                                    d17a> typso_2_3 ~213,
                                    typmed==22 & d14ba> typso_2_3 ~ 221,
                                    typmed==22 & d16a> typso_2_3 ~ 222,
                                    TRUE~ as.numeric(typlow)))

june_so <- june_so %>%rowwise %>% 
  dplyr::mutate(typlow = case_when(
    typmed==22 & !(typlow %in%c(221, 222)) ~ 223,
    typmed==23 & g05aa>typso_2_3 ~ 232,
    typmed==23 & !(typlow %in%c(231, 232)) ~ 233,
    typmed==36 ~ 361,
    typmed==45 ~ 450,
    typmed==46 ~ 460,
    typmed==47 ~ 470,
    typmed == 48 & j09tot> typgz_2_3 & GL> typgz_p4x ~ 481,
    typmed == 48 & P46> typgz_1_3 & j09tot> typgz_1_3 & GL> typgz_p4x ~482,
    typmed == 48 & j10tot> typgz_2_3 & GL> typgz_p4x ~ 483,
    typmed == 48 & !(typlow %in%c(481, 482, 483))~ 484,
    typmed==51 & j12a> typso_2_3 ~ 511,
    typmed==51 & sum(j11a,j13a, na.rm=TRUE)> typso_2_3 ~512,
    typmed==51 ~ 513,
    typmed==52 & j15a> typso_2_3 ~521,
    typmed==52 & sum(j14a, j16a, na.rm=TRUE)>typso_2_3 ~ 522,
    typmed==52 ~ 523,
    typmed==53~ 530,
    typmed==61 & (P2>typso_1_3 & P3 > typso_1_3) ~611,
    typmed==61 & (P2>typso_1_3 & P1 > typso_1_3)~612,
    typmed==61 & (P1>typso_1_3 & P3 > typso_1_3) ~614,
    typmed==61 & (P1>typso_1_3 & P2<=typso_1_3 & P3<= typso_1_3)~615,
    typmed==61 ~ 616,
    typmed==73 & P45>typgz_1_3 & j07a> (0.5*P45) ~ 731,
    typmed==73 ~ 732,
    typmed==74 & P5>typso_1_3 & P45>typgz_1_3 & j07a> (0.5*P45) ~ 741,
    typmed==74 ~742,
    typmed==83 & P45>typgz_1_3 & j07a> (0.5*P45) & P45<P1 ~ 831,
    typmed==83 & P45>typgz_1_3 & j07a> (0.5*P45) & P45>=P1 ~ 832,
    typmed==83 & P4<P1 & typmed != 831 ~ 833,
    typmed==83 ~ 834,
    typmed==84 & P1>typso_1_3 & P5 > typso_1_3 ~ 841,
    typmed==84 & P3>typso_1_3 & P4 > typso_1_3 ~ 842,
    typmed==84 ~ 844,
    typmed==90 & sumso ==0 ~ 900,
    TRUE~ as.numeric(typlow))
    
    
    
  )



#reassigns previous typmed values (e.g. no more typmed=48 after this) 

june_so <- june_so %>%rowwise %>% 
  dplyr::mutate(typmed = case_when(typmed==46 & lfatemp==1 ~ 9946,
                                   typmed==46 & lfatemp==2 ~ 9946,
                                   typmed==46 & lfatemp==3 ~ 7746,
                                   typmed==47 & lfatemp==1 ~ 9947,
                                   typmed==47 & lfatemp==2 ~ 9947,
                                   typmed==47 & lfatemp==3 ~ 7747,
                                   typmed==48 & lfatemp==1 ~ 9948,
                                   typmed==48 & lfatemp==2 ~ 9948,
                                   typmed==48 & lfatemp==3 ~ 7748,
                                   TRUE~ as.numeric(typmed)
                                   
                                   
  )
  )




# Mappings for New Robust Types  ------------------------------------------


  

  june_so <- june_so %>%rowwise %>% 
    dplyr::mutate(robust_new = case_when(typmed==15 ~ 1,
                                         typmed==16 & typlow !=166   ~ 2,
                                         typmed==16 & typlow ==166 & FCP1> sum(d10a, d14aa, d20a, na.rm = TRUE) ~ 10))
  
  
  june_so <- june_so %>%rowwise %>% 
    dplyr::mutate(robust_new = case_when(typmed==15 ~ 1,
                                         typmed==16 & typlow ==166 & FCP1< sum(d10a, d14aa, d20a, na.rm = TRUE) ~ 2,
                                         
                                         typhigh==2 ~ 3,
                                         typhigh==3 ~ 3,
                                         typmed==45 ~ 6,
                                         typmed==9946 ~ 7,
                                         typmed==9947 ~ 7,
                                         typmed==9948 & typlow != 484 ~7,
                                         typmed == 9948 & typlow == 484 & j01a < 0.5*sumso ~7,
                                         typmed == 9948 & typlow == 484 & j01a >= 0.5*sumso ~9,
                                         typmed == 7746 ~8,
                                         typmed == 7747 ~8,
                                         typmed == 7748 & typlow != 484 ~8,
                                         typmed == 7748 & typlow == 484 & j01a < 0.5*sumso ~8,
                                         typmed == 7748 & typlow == 484 & j01a >= 0.5*sumso ~9,
                                         typmed == 51 ~ 4,
                                         typmed == 52 ~ 5,
                                         typmed == 53 & P51 >= P52 ~4,
                                         typmed == 53 & P51 < P52 ~5,
                                         typhigh==6 & typlow == 611 ~ 3,
                                         typhigh==6 & typlow == 616 & sum(P2, P3, na.rm = TRUE)> typso_2_3 ~ 3,
                                         typhigh==6 & typlow == 616 & sum(P2, P3, na.rm = TRUE)<=typso_2_3 ~ 9,
                                         typhigh==6 &  !(typlow %in% c(611, 616)) ~ 9,
                                         typhigh==7 ~ 9,
                                         typhigh==8 ~ 9,
                                         typhigh==9 ~ 11,
                                         TRUE~ as.numeric(robust_new)
                                         
                                         
                                         
                                         
    )
    )
  
  
  

  june_so <- june_so %>%rowwise %>% 
    dplyr::mutate(newstrata = case_when(	sumso >=0 & sumso <  10000 ~(robust_new-1)*6 + 1,
                                  sumso >= 10000 & sumso <  20000 ~(robust_new-1)*6 + 2,
                                  sumso >= 20000 & sumso < 100000 ~(robust_new-1)*6 + 3,
                                  sumso >=100000 & sumso < 250000 ~(robust_new-1)*6 + 4,
                                  sumso >= 250000 &  sumso < 500000 ~(robust_new-1)*6 + 5,
                                  sumso >= 500000 ~(robust_new-1)*6 + 6),
           size = case_when(sumso <=0 ~ 0,
                            sumso > 0 & sumso <  10000 ~ 1,
                            sumso >= 10000 & sumso <  20000 ~ 2,
                            sumso >= 20000 & sumso < 100000 ~ 3,
                            sumso >=100000 & sumso < 250000 ~4,
                            sumso >= 250000 &  sumso < 500000 ~ 5,
                            sumso >= 500000 ~ 6)
                                  
      
    
    )
           

# Robust new summary ------------------------------------------------------

robust_new_summary <- june_so %>% select(sumso, item50, robust_new)           

  #get label names from agscens format         
robust_name <- agscens_format %>% 
  filter(FMTNAME == "ROBUST_NEW")%>% 
  select(FMTNAME, START, LABEL)

#lowercase column names
names(robust_name) <- tolower(names(robust_name))

robust_name<- robust_name %>% ungroup %>%  
  filter(fmtname=="ROBUST_NEW") %>%
  dplyr::rename(robust_new = "start") %>% 
  select(robust_new, label)

robust_name$robust_new <- as.numeric(robust_name$robust_new)

#join
robust_new_summary  <- full_join(robust_new_summary, robust_name, by = "robust_new") 

robust_new_1 <- robust_new_summary %>% group_by(label) %>% dplyr::summarise(count = n(), 
                                                                            sum_item50 = sum(item50, na.rm = TRUE),
                                                                            sum_sumso = sum(sumso, na.rm= TRUE))
all_robust <- plyr::numcolwise(sum)(robust_new_1)
rownames(all_robust) <- "all"
robust_new_summary <- bind_rows(robust_new_1, all_robust)


# Calculate SLR -----------------------------------------------------------

#for QA with june_21
lfa_split <-   address_2023 %>% 
  select(c(Parish,
           Holding,
           TotArea,
           TotDaArea,
           TotOwnArea,
           TotSdaArea,
           TotNonLfaArea,)) 

lfa_split <- lfa_split %>%   dplyr::rename(parish = "Parish",
         holding = "Holding")

june_slr <- left_join(june_so, lfa_split, by = c("parish", "holding"))


june_slr <- june_slr %>% rowwise %>% 
  dplyr::mutate(
    r334= sum(item19,item23,item21, na.rm = TRUE),
    item94= case_when(is.na(item94) ~ 0,
                      TRUE ~ as.numeric(item94)),
    j19=item94,
    slr_d01 = 18.0,
    slr_d03	= 18.0,
    slr_d04	= 18.0,	
    slr_d05	= 18.0,		
    slr_d08 = 18.0,
    slr_d09 = 16.0, 
    slr_d10 = 110.0,
    slr_d12 = 6.0,
    slr_d14 = 280.0,
    slr_d15 = 7000.0,           
    slr_d16 = 1900.0,
    slr_d17 = 13000.0,
    slr_d18a = 3.1,
    slr_d18b = 6.0,
    slr_d19 = 15.0,
    slr_d20 = 15.0,
    slr_d21 = 2.9,
    slr_f01 = 3.1,
    slr_f02 = 1.5,
    slr_f03 = 2.9,
    slr_g01 = 425.0,
    slr_g05 = 1900, 
    slr_r334 = 16.0,
    slr_it52 = 12,
    slr_it53 = 500,
    slr_j01 =   40.0,
    slr_j02 =   12.0,
    slr_j03 =   12.0,
    slr_j04 =   12.0,
    slr_j05 =   12.0,
    slr_j06 =   12.0,              
    slr_j07 =   42.0,
    slr_j08	=	26.0,	
    slr_lj09a = 3.7,
    slr_nj09a = 5.2, 
    slr_lj09b=3.1,
    slr_nj09b=2.9,
    slr_j10 =   12.0,
    slr_j11 =   0.2,
    slr_j12 =   28.0,
    slr_j13 =   2.3, 
    slr_j14 =   0.09,
    slr_j15 =   0.36,
    slr_j16 =   0.1,
    slr_j19 =   15.0,
    d01a_slr    = d01 * slr_d01,
    d03a_slr   = d03  * slr_d03,	
    d04a_slr   = d04  * slr_d04,
    d05a_slr   = d05  * slr_d05,
    d08a_slr   = d08 * slr_d08,
    d09a_slr   = d09 * slr_d09,
    d10a_slr   = d10 * slr_d10,
    d12a_slr   = d12 * slr_d12,
    r334a_slr  = r334 * slr_r334,
    it52a_slr  = item52 * slr_it52,
    it53a_slr  = item53 * slr_it53,
    d14 = sum(item55, 
              item56, 
              item2323, 
              item59,
              item60, 
              item61, 
              item63,
              item64, 
              item65, 
              item66, 
              na.rm = TRUE),
    d14a_slr    = d14 * slr_d14,
    d15a_slr     = d15  * slr_d15,
    d16a_slr     = d16  * slr_d16,
    d17a_slr     = d17  * slr_d17,
    d18aa_slr    = d18a * slr_d18a,
    #following lines are commented out because variables don't exist. SAS code is erroneous
   # d18ba_slr    = d18b * slr_d18b,
    #d19a_slr     = d19  * slr_d19,
    d20a_slr     = d20  * slr_d20,
    d21a_slr     = d21  * slr_d21,
    f01a_slr     = f01 * slr_f01,
    f02a_slr     = f02 * slr_f02,
    f03a_slr     = f03 * slr_f03,
    g01aa_slr    = g01a * slr_g01,
    g05aa_slr    = g05a * slr_g05,
    j01a_slr    = j01 * slr_j01,
    j02a_slr    = j02 * slr_j02,
    j03a_slr    = j03 * slr_j03,
    j04a_slr    = j04 * slr_j04,
    j05a_slr    = j05 * slr_j05,
    j06a_slr    = j06 * slr_j06,
    j07a_slr    = j07 * slr_j07,
    j08a_slr    = j08  * slr_j08,
    lfa_split = sum(sum(TotDaArea,
                        TotSdaArea, 
                        na.rm=TRUE)/
                      sum(TotDaArea, 
                          TotSdaArea, 
                          TotNonLfaArea, 
                          na.rm= TRUE)),
    lfa_sheep = sum(sum(TotDaArea,
                        TotSdaArea, 
                        na.rm=TRUE)/
                      TotArea),
    non_lfa_sheep = sum(1, -(lfa_sheep), na.rm=TRUE),
    Ewerams_nlfa=sum((sum(item139,
                          item140, 
                          na.rm=TRUE)*non_lfa_sheep)*slr_nj09a),
    Ewerams_lfa=sum((sum(item139,
                         item140, 
                         na.rm=TRUE)*lfa_sheep)*slr_lj09a),
    Others_nlfa= sum((sum(item141,
                          item143,
                          item144,
                          na.rm=TRUE)*non_lfa_sheep)*slr_nj09b),
    Others_lfa= sum((sum(item141,
                         item143, 
                         item144,
                         na.rm=TRUE)*lfa_sheep)*slr_lj09b,
                    na.rm=TRUE),
    j09aa_slr=sum(Ewerams_nlfa,Ewerams_lfa,
                  na.rm=TRUE),
    j09ba_slr=sum(Others_nlfa,Others_lfa,
                  na.rm=TRUE),
    j10a_slr    = j10tot * slr_j10,
    j11a_slr    = j11 * slr_j11,
    j12a_slr    = j12 * slr_j12,
    j13a_slr    = j13 * slr_j13,
    j14a_slr    = j14 * slr_j14,
    j15a_slr    = j15 * slr_j15,
    j16a_slr    = j16 * slr_j16,
    j19a_slr    = j19 * slr_j19,
    p111_slr = sum(d01a_slr, 
                   d03a_slr,
                   d04a_slr, 
                   d05a_slr, 
                   d08a_slr,
                   na.rm=TRUE),
    p121_slr = d10a_slr,
    p1_slr = sum(p111_slr, 
                 p121_slr, 
                 d09a_slr,
                 r334a_slr,
                 d14a_slr,
                 #d19a_slr,
                 d20a_slr,
                 it52a_slr,
                 it53a_slr,
                 na.rm=TRUE),
    p2_slr = sum(d15a_slr,
                 d16a_slr,
                 d17a_slr, 
                 g05aa_slr,
                 na.rm=TRUE),
    p3_slr = g01aa_slr,
    p41_slr = sum(j02a_slr, 
                  j04a_slr,
                  j06a_slr, 
                  j07a_slr,
                  na.rm=TRUE),
    p42_slr = sum(p41_slr, 
                  j03a_slr, 
                  j05a_slr, 
                  j08a_slr,
                  na.rm=TRUE),
    p4_slr = sum(j01a_slr,
                 p42_slr, 
                 j09aa_slr,
                 j09ba_slr, 
                 j10a_slr,
                 na.rm=TRUE),
    p51_slr = sum(j11a_slr,
                  j12a_slr, 
                  j13a_slr,
                  na.rm=TRUE),	
    p52_slr = sum(j14a_slr,
                  j15a_slr, 
                  j16a_slr,
                  na.rm=TRUE),
    p5_slr = sum(p51_slr,
                 p52_slr,
                 na.rm=TRUE),
    p6_slr = sum(d18aa_slr, 
                 #d18ba_slr,
                 #d12_slr doens't exist...SAS 
                 d12a_slr,
                 f01a_slr, 
                 f02a_slr, 
                 f03a_slr, 
                 j19a_slr, 
                 d21a_slr,
                 na.rm=TRUE),
    slr2006 = sum(p1_slr,
                  p2_slr,
                  p3_slr,
                  p4_slr,
                  p5_slr,
                  p6_slr,
                  na.rm=TRUE),
    slr2006_error = case_when(is.na(slr2006) ~ 1,
                              TRUE ~ 0),
    slr = slr2006/1900,
    full_occ1 = case_when(slr2006 == 0 & item177>=0~1),
    part_occ1 = case_when(slr2006 == 0 & item178>=0~1),
    cas_occ1 = case_when(slr2006 == 0 & item179>=0~1),
    full_occ2 = case_when(slr2006 == 0 & item182>=0~1),
    part_occ2 = case_when(slr2006 == 0 & item183>=0~1),
    cas_occ2 = case_when(slr2006 == 0 & item184>=0~1),
    full = case_when(slr2006 == 0 ~sum(full_occ1,
              full_occ2,
              item1714,
              item1715,
              item1716,
              item1717,
              item192,
              item193, 
              na.rm=TRUE)),
    part = case_when(slr2006 == 0 ~ sum(part_occ1,
             part_occ2,
             item1718,
             item194,
             item195,
             item1719,
             item196,
             item197,
             na.rm= TRUE)),
    casual = case_when(slr2006 == 0 ~ sum(cas_occ1, 
               cas_occ2,
               item198,
               item199,
               na.rm=TRUE)),
    nonreg = case_when(slr2006 == 0 ~ sum(item2712,
               item2066, 
               item2511,
               na.rm=TRUE)),
    full_slr =case_when(slr2006 == 0 ~full),
    part_slr = case_when(slr2006 == 0 ~ 0.5*part),
    casual_slr = case_when(slr2006 == 0 ~ 0.25*casual),
    nonreg_slr = case_when(slr2006 == 0 ~ nonreg*8/1900),
    slr=case_when(slr2006 == 0 ~sum(full_slr,
                                    part_slr,
                                    casual_slr,
                                    nonreg_slr),
                                    TRUE ~ as.numeric(slr))
                  
    
    
  )

#duplicate checks
dup_slr <- duplicated(june_slr)
summary(dup_slr)
dup_slr<- june_slr[duplicated(june_slr), ]
summary(unique(june_slr)==june_slr)




# Add SO, farm types and SLR to dataset, calculate main_min flag,  --------
# try with 2019 for QA
address_2023 <- address_2023 %>% group_by(Parish, Holding) %>% filter(Holding_Classification_Id == 6) %>% select(-contains("ID", ignore.case=FALSE))
address_2021 <- address_2021 %>%  group_by(Parish, Holding) %>% filter(Holding_Classification_Id == 6)%>% select(-contains("ID", ignore.case=FALSE))
address_2020 <- address_2020 %>%  group_by(Parish, Holding) %>%  filter(Holding_Classification_Id == 6)%>% select(-contains("ID", ignore.case=FALSE))

#format before add to match 2023 address file
address_2021 <- address_2021 %>% dplyr:: mutate(HoldingDsc6 = "",
                                       HoldingDsc7 = "",
                                       HoldingDsc8 = "",
                                       HoldingDsc9 = "",
                                       HoldingDsc10 = "",
                                       HoldingDsc11 = "",
                                       HoldingDsc12 = "",
                                       HoldingDsc13 = "",
                                       HoldingDsc14 = "",
                                       HoldingDsc15 = "") 


address_2020 <- address_2020 %>% dplyr:: mutate(HoldingDsc6 = "",
                                                HoldingDsc7 = "",
                                                HoldingDsc8 = "",
                                                HoldingDsc9 = "",
                                                HoldingDsc10 = "",
                                                HoldingDsc11 = "",
                                                HoldingDsc12 = "",
                                                HoldingDsc13 = "",
                                                HoldingDsc14 = "",
                                                HoldingDsc15 = "") 



poultry_add <- bind_rows(address_2023, address_2021, address_2020)

#remove duplicates
poultry_add <- unique(poultry_add) %>% dplyr::arrange(by_group= TRUE)

duplicate_ph <- poultry_add %>% select(Parish, Holding)
duplicate_ph <- duplicated(duplicate_ph)

#subset to isolate duplicated holdings
duplicate_poultry <- poultry_add[duplicate_ph, ]

#unique holdings
unique_poultry <- setdiff(poultry_add, duplicate_poultry)



#select cols
brn <- brn %>% select(parish,
                      holding,
                      brn)

slr <- june_slr %>% select(parish, 
                           holding,
                           sumso,
                           robust_new,
                           typhigh,
                           typmed,
                           typlow,
                           slr, 
                           sgm2000,
                           EC_SGM,
                           ecsgm1,
                           ectype1,
                           slr2006,
                           newstrata,
                           size) 

poultry_add <- unique_poultry %>%
  dplyr::rename(parish = "Parish",
                holding = "Holding") %>% 
  select(parish, holding) %>% 
  dplyr::mutate(large_poultry = 1)

june_brn <- dplyr::left_join(x = june_temp, y= slr, by = c("parish", "holding"))
june_brn <- dplyr::left_join(june_brn, poultry_add, by = c("parish", "holding"))
june_brn <- dplyr::left_join(june_brn, brn, by = c("parish", "holding"))


# BRN Dataset-------------------------------------------------------------


#BRN summary
june_brn_summary <- june_brn %>% 
  filter(completedata==1 & brn >0) %>% 
  select(brn, item50,
         item170,
         item177,
         item182,
         item1714,
         item1715,
         item1716,
         item1717,
         item192,
         item193,
         item80,
         item81,
         item1710,
         item82,
         item83,
         #item87,
         #item2713, 
         item2707,
         item2556, 
         item2557,
         item2836 ,
         #item2036,
         #item2714,
         #item2708,
         #item2037,
         #item2715,
         #item2709,
         #item1711, 
         #item2716,
         #item2710,
         #item1943
         #item2717,
         #item2711,
         item94,
         sumso)

june_brn_summary <- june_brn_summary %>% group_by(brn) %>% dplyr::summarise(count = n(), sum_= across(everything(),  ~ sum(., na.rm = TRUE)))
june_brn_summary$sum_ <- june_brn_summary$sum_ %>%  select(-count) 
names(june_brn_summary$sum_) <- paste0("sum_", names(june_brn_summary$sum_))

june_brn_summary <- cbind(june_brn_summary, june_brn_summary$sum_) %>% select(-sum_) %>% filter(!is.na(brn))



june_classification <- full_join(june_brn, june_brn_summary, by = "brn")


june_classification <- june_classification %>% group_by(parish, holding) %>% rowwise %>% 
  dplyr::mutate(num_holdings = count,
                cowsunder1 = ifelse(completedata==1, sum(sum(CTS301,
                                                            CTS302, 
                                                            CTS309,
                                                            na.rm= TRUE)*0.4),  0),
                cows1to2 = ifelse(completedata==1, sum(sum(CTS303,
                                                          CTS304,
                                                          CTS310, 
                                                          na.rm= TRUE) * 0.7),  0),
                bulls2plus = ifelse(completedata==1, CTS311, 0),
                heifs2plus = ifelse(completedata==1, sum(sum(CTS307,
                                                             CTS308,
                                                             na.rm= TRUE) * 0.8), 0),
                dairy = ifelse(completedata==1, CTS306, 0),
                othcows = ifelse(completedata==1,sum(CTS305 * 0.8), 0),
                # sheepgoat = ifelse(completedata==1,sum(sum(#item98,
                #                                            #item1712,
                #                                            #item1713,
                #                                            #item145,
                #                                            na.rm= TRUE) * 0.1),0),
                sheepgoat = ifelse(completedata==1, sum(sum(item27780,
                                                            item145,
                                                            na.rm= TRUE) * 0.1),0),
                
                # horses = ifelse(completedata==1, sum(sum(#item95,
                #                                          item96, 
                #                                          na.rm= TRUE) * 0.8),0),
                horses = ifelse(completedata==1, sum(sum(item27775) *0.8), 0),                                        
                #weepigs = ifelse(completedata==1,sum(item156 * 0.027)),
                weepigs = ifelse(completedata==1,sum(item27770 * 0.027), 0),
                breedsows = ifelse(completedata==1,sum(sum(item146,
                                                           item147,
                                                           item148, 
                                                           item150, 
                                                           na.rm= TRUE) * 0.5), 0), 
                # othpigs = ifelse(completedata==1, sum(sum(item149,
                #                                           item151, 
                #                                           #item152,
                #                                           #item153,
                #                                           #item154, 
                #                                           #item155,  
                #                                           na.rm= TRUE) * 0.3)), 
                othpigs = ifelse(completedata==1, sum(sum(item149,
                                                          item151, 
                                                          item27760,
                                                          item27765,
                                                          na.rm= TRUE) * 0.3), 0), 
                broilers = ifelse(completedata==1, sum(item164 * 0.007), 0),  
                layers = ifelse(completedata==1, sum(sum(item158,
                                                         item159, 
                                                         item160, 
                                                         item161,
                                                         item162, 
                                                         item163, 
                                                         na.rm= TRUE) * 0.014), 0), 
                othpoultry =ifelse(completedata==1, sum(sum(item1708,
                                                            item2038,
                                                            item2039,
                                                            item167,
                                                            na.rm= TRUE) * 0.03), 0),
                lsu = sum(cowsunder1,
                          cows1to2, 
                          bulls2plus,
                          heifs2plus, 
                          dairy,
                          othcows,
                          sheepgoat, 
                          horses,
                          weepigs,
                          breedsows,
                          othpigs,
                          broilers,
                          layers,
                          othpoultry, 
                          na.rm=TRUE))

#Deal with NA before domestic
june_classification <- june_classification %>% mutate(large_poultry = ifelse(is.na(large_poultry), 0, as.numeric(large_poultry)))

june_classification <- june_classification %>% rowwise %>% 
  dplyr::mutate(domestic =ifelse(completedata==1 &
                                      ((item12 < 0.5 | is.na(item12)) & (item50 < 0.5 | is.na(item50))) & 
                                      lsu <= 1 & 
                                      sum(item1714,
                                          item1715,
                                          item1716,
                                          item1717,
                                          item192,
                                          item193,
                                          item1718,
                                          item194, 
                                          item195, 
                                          item1719,
                                          item196, 
                                          item197,
                                          item198, 
                                          item199,
                                          item2712, 
                                          item2066, na.rm =TRUE) <=0 & 
                                      sum(item177,
                                          item178,
                                          item182,
                                          item183, na.rm = TRUE) <= 0 &
                                      (item94 <= 0 | is.na(item94)) &
                                      (item170 <= 20 | is.na(item170)) &
                                      sum(item40,
                                          item2321,
                                          na.rm=TRUE) <= 0 &
                                     !(land_data %in% c("saf", "both")) & 
                                      large_poultry != 1 &
                                      (num_holdings <= 1 | is.na(num_holdings)), 1, 0))




june_classification <- june_classification %>% rowwise %>% 
  dplyr::mutate(forestry_only = ifelse(completedata == 1 & domestic == 0 &
                                         sum(item48, item49, na.rm = TRUE) == item50 &
                                         item48 > 0, 1, 0 ),
                main_min = ifelse(completedata ==1 & domestic ==1 , "domestic", 
                                     ifelse(completedata==1 & forestry_only == 1,  "forestry", NA)))


june_classification <- june_classification %>% rowwise %>% 
  dplyr::mutate(main_min = ifelse(completedata == 1 & !(main_min %in% c("domestic", "forestry")),
                                  ifelse(num_holdings > 1,  
                                         ifelse((sum_item50 < 1 | (sum_sumso <= 4400) | is.na(sum_sumso))	& 
                                                  sum_item170 < 200 
                                                & sum(sum_item177, 
                                                      sum_item182,
                                                      sum_item1714,
                                                      sum_item1715,
                                                      sum_item1716,
                                                      sum_item1717,
                                                      sum_item192,
                                                      sum_item193,  na.rm=TRUE) <= 0 & 
                                                  sum(sum_item80,
                                                      sum_item81,
                                                      sum_item1710,
                                                      sum_item82,
                                                      sum_item83, 
                                                      #sum_item87,
                                                      #sum_item2713, 
                                                      sum_item2707,
                                                      sum_item2556, 
                                                      sum_item2557, 
                                                      sum_item2836,
                                                      #sum_item2036,
                                                      #sum_item2714,
                                                      #sum_item2708,
                                                      #sum_item2037,
                                                      #sum_item2715,
                                                      #sum_item2709, 
                                                      #sum_item1711,
                                                      #sum_item2716,
                                                      #sum_item2710,
                                                      #sum_item1943,
                                                      #sum_item2717,
                                                      #sum_item2711,
                                                      na.rm=TRUE) <= 0 & 
                                                  sum_item94 < 100,
                                                "minor", 
                                                "main"),
                                         ifelse(num_holdings <= 1 | is.na(num_holdings),
                                                ifelse((item50 < 1 | (sumso <= 4400) | is.na(sumso))  & 
                                                         item170 < 200 & 
                                                         sum(item177,
                                                             item182,
                                                             item1714,
                                                             item1715,
                                                             item1716, 
                                                             item1717,
                                                             item192,
                                                             item193,
                                                             na.rm=TRUE) <= 0 & 
                                                         sum(item80,
                                                             item81, 
                                                             item1710,
                                                             item82, 
                                                             item83, 
                                                             #item87,
                                                             #item2713, 
                                                             item2707,
                                                             item2556, 
                                                             item2557,
                                                             item2836 ,
                                                             #item2036,
                                                             #item2714,
                                                             #item2708,
                                                             #item2037,
                                                             #item2715,
                                                             #item2709,
                                                             #item1711, 
                                                             #item2716,
                                                             #item2710,
                                                             #item1943
                                                             #item2717,
                                                             #item2711,
                                                             na.rm=TRUE) 
                                                       <= 0 &
                                                         item94 < 100, 
                                                       "minor", 
                                                       "main")
                                         )
                                  ), 
                                  as.character(main_min)),
                survtype_new = case_when(completedata == 1 & land_data %in% c("saf", "both") ~ 1,
                                         completedata == 1 & main_min == "main" ~ 2,
                                         completedata != 1 ~ 0,
                                         TRUE ~ 3)
                
                
  )

june_classification <- june_classification %>%  mutate(main_min = ifelse(completedata !=1, NA, as.character(main_min)))


# Save final dataset ------------------------------------------------------

                

june_classification <- june_classification %>% select(parish,
                                                      holding,
                                                      brn,
                                                      num_holdings,
                                                      lsu,
                                                      main_min,
                                                      domestic,
                                                      forestry_only,
                                                      survtype_new)                
    




june_final_23 <- left_join(june_temp, june_classification, by = c("parish", "holding"))
june_final_23 <- left_join(june_final_23, slr, by = c("parish", "holding"))


write_dataframe_to_db(
  server = server,
  database = database,
  schema = schema,
  table_name = "JAC23_final_dataset",
  dataframe = june_final_23,
  append_to_existing = FALSE,
  versioned_table = FALSE,
  batch_size = 10000
)  
