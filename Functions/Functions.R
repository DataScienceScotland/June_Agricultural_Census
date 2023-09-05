# This script contains all the functions for the June project and is loaded at the start of all the June scripts. 
# Some functions may need updating yearly. As they were written for the 2021 data, some may be updated for 2023.
# Modified by Lucy Nevard 30.05.23

# General functions -------------------------------------------------------


# Load Rda with a name


loadRData <- function(fileName) {
  # loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}


# The following two functions are for looking at how much memory is taken up by each object in the environment
# Taken from online
# Main function

.ls.objects <- function(pos = 1, pattern, order.by,
                        decreasing = FALSE, head = FALSE, n = 5) {
  napply <- function(names, fn) {
    sapply(names, function(x) {
      fn(get(x, pos = pos))
    })
  }
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.prettysize <- napply(names, function(x) {
    capture.output(format(utils::object.size(x), units = "auto"))
  })
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x) {
    as.numeric(dim(x))[1:2]
  }))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
  names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
  if (!missing(order.by)) {
    out <- out[order(out[[order.by]], decreasing = decreasing), ]
  }
  if (head) {
    out <- head(out, n)
  }
  out
}


# Shorthand function for easy access

lsos <- function(..., n = 10) {
  .ls.objects(..., order.by = "Size", decreasing = TRUE, head = TRUE, n = n)
}


# flatten function, useful for flattening a list into dataframes. Taken from online.

flatten <- function(data) {
  ListCols <- sapply(data, is.list)
  cbind(data[!ListCols], t(apply(data[ListCols], 1, unlist)))
}
# Functions used in A -----------------------------------------------------


# Create "line" variable to index by LPID. Number gives instance of each LPID.


LPID_index <- function(x) {
  setDT(x)[, line := seq(1, .N), by = lpid]
}


# Rename selected variables in permanent and seasonal datasets
rename_perm_seas_vars <- function(x) {
  dplyr::rename(x,
    brn = "business_reference_number",
    mlc = "main_location_code",
    slc = "location_code",
    fid = lpid,
    field_area = "land_parcel_area",
    lfass_eligible = lfass,
    eligible_area = "bps_eligible_area",
    llo = "land_leased_out",
    land_use = "land_use",
    land_use_area = "land_use_area",
    bps_claimed_area = "bps_claimed_area"
  )
}

# Create new variables for permanent and seasonal datasets
new_perm_seas_vars <- function(x) {
  mutate(x,
    slc = substr(slc, 4, 11),
    mlc = substr(mlc, 4, 11),
    land_use = str_replace_all(land_use, "_", "-"),
    sfp_area = bps_claimed_area,
    sfp_code = land_use,
    other_area = (land_use_area - bps_claimed_area),
    other_code = land_use
  )
}

# Create parish and holding from slc in permanent
parishholdingperm<-function(x) {
  mutate(x,
         parish = str_remove(substr(slc, 1, 3), "^0+"),
         holding = str_remove(substr(slc, 5, 8), "^0+")
  )
}

# Create parish and holding from mlc in seasonal

parishholdingseas<-function(x) {
  mutate(x,
         parish = str_remove(substr(mlc, 1, 3), "^0+"),
         holding = str_remove(substr(mlc, 5, 8), "^0+")
  )
}

# Rename selected variables in scheme dataset
rename_scheme_vars <- function(x) {
  dplyr::rename(x,
         brn = "business_reference_number",
         mlc = "main_location_code",
         slc = "location_code",
         fid = lpid
  )
}

# Create new variables for scheme dataset
new_scheme_vars <- function(x) {
  mutate(x,
    slc = substr(slc, 4, 11),
    mlc = substr(mlc, 4, 11),
    parish = str_remove(substr(slc, 1, 3), "^0+"),
    holding = str_remove(substr(slc, 5, 8), "^0+"),
    scheme_option_code = str_replace_all(scheme_option_code, "_", "-") # not sure this line is necessary - there are no underscores or hyphens?
  )
}

# Find missing observations in permanent and seasonal.

missing_obs <- function(x) {
  subset(
    x,
    is.na(brn) == TRUE | is.na(mlc) == TRUE | is.na(line) == TRUE | is.na(slc) == TRUE | is.na(fid) == TRUE
  )
}


# Find missing field areas in permanent and seasonal
missing_field_area <- function(x) {
  x[field_area == 0 | is.na(field_area)]
}


# Find missing eligible areas in permanent and seasonal
missing_eligible_area <- function(x) {
  x[is.na(eligible_area)]
}


# Remove NAs from datasets

cleaned_datasets <- function(x) {
  filter(
    x,
         complete.cases(brn) | complete.cases(mlc) | complete.cases(slc)
  )
}




# Functions for B --------------------------------------------------------

# B6 


# Correct areas and change codes. Check if this needs updating every year. 

change_codes<-function(x) {
  mutate(x,
         other_area =
           ifelse(land_use_area < bps_claimed_area, "0", other_area),
         sfp_area =
           ifelse(land_use_area < bps_claimed_area, bps_claimed_area, sfp_area),
         land_use_area =
           ifelse(land_use_area < bps_claimed_area, bps_claimed_area, land_use_area),
         slc = ifelse(slc == "", mlc, slc),
         sfp_code =
           recode(as.factor(sfp_code), "UNSP" = "EXCL"),
         other_code =
           recode(as.factor(other_code), "UNSP" = "EXCL")
  )
}


# create new variables for "other" land in permanent

newvarsother<-function(x)  {
  mutate(x,
        claimtype = "OTHER",
        other_code=as.character(other_code),
        prefix = substr(other_code, 1, 4),
        code = other_code,
        crops = other_code,
        area = other_area,
        llo =
          ifelse(prefix == "LLO-", "Y", ifelse(other_code == "LLO", "Y", llo)),
        other_code =
          ifelse(prefix == "LLO-", substring(other_code, 5), other_code),
        other_code =
          ifelse(other_code == "", "OTH-LAND", other_code)
)
}

# Create new variables for "sfp" land in permanent 

newvarssfp<-function(x)  {
  mutate(x,
         sfp_code = as.character(sfp_code),
         claimtype = "SFPS",
         prefix = substr(sfp_code, 1, 4),
         code = sfp_code,
         crops = sfp_code,
         area = sfp_area,
         llo =
           ifelse(prefix == "LLO-", "1", ifelse(sfp_code == "LLO", "Y", llo)),
         sfp_code =
           ifelse(prefix == "LLO-", substring(sfp_code, 5), sfp_code),
         sfp_code =
           ifelse(sfp_code == "", "OTH-LAND", sfp_code),
         line =
           ifelse(other_area > 0, line + 0.01, line)
  )
}




# Create new variables in seasonal 


newvarsseas<-function(x)  {
  mutate(x,
  claimtype = "SFPS",
  llo = "N",
  code = sfp_code,
  crops = sfp_code,
  area = sfp_area,
  slc = ifelse(slc == "", mlc, slc)
)
}

# Correct variable types for perm

perm_variables<-function(x)    {
  mutate(x,
  parish=as.numeric(parish),
  holding=as.numeric(holding),
  code=as.factor(code),
  crops=as.factor(crops),
  area=round(as.numeric(area),3),
  line=as.numeric(line)
)
}
# Correct variable types for seas

seas_variables<-function(x)    {
  mutate(x,
    sfp_code=as.factor(sfp_code),
    parish=as.numeric(parish),
    holding=as.numeric(holding),
    code=as.factor(code),
    crops=as.factor(crops),
    line=as.numeric(line),
    area=round(as.numeric(area),3)
  )
}


#B7

# Create "line" variable to index by fid (field ID).

fid_index   <-  function(x) {
  setDT(x)[, line := seq(1, .N), by = fid]
}

# create sum variables in df to check areas 

summarycheckarea<-function(x) {
  dplyr::summarize(x,
    sum_area = sum(area),
    sum_field = sum(field_area),
    sum_eligible = sum(eligible_area),
    max_field = max(field_area),
    var_field = var(field_area),
    flag1 = max(flag1),
    flag7 = max(flag7)
  )
}


mutateareamismatches<-function(x) {
mutate(x,
  diff = round(max_field - sum_area, 3),
  ratio = round(sum_area / max_field, 3)
) 
}


#B8


mutatellolfass<-function(x) {
  mutate(x,
           llo_area =
             ifelse(llo == "Y", area, 0),
           lfass_area =
             ifelse((lfass_eligible == "Y" & claimtype == "SFPS"), area, 0))
}

summaryfids<-function(x) {
  summarize(x,
  mlc = unique(mlc),
  slc = unique(slc),
  brn = unique(brn),
  area = sum(area),
  llo_area = sum(llo_area),
  lfass_area = sum(lfass_area),
  field_area = max(field_area),
  eligible_area = max(eligible_area),
  .groups = "keep"
)
}

aggregatefids<-function(x){
  summarize(x,
  area = sum(area),
  llo_area = sum(llo_area),
  lfass_area = sum(lfass_area),
  mlc = unique(mlc),
  slc = unique(slc),
  brn = unique(brn),
  .groups = "keep"
)
}


# B9

othercropscodes<-function(x) {
  mutate(x,
    ncode =
      ifelse(
        cens_code == "item41",
        ifelse(
          code == "ALMS",
          "Almonds",
          ifelse(
            code == "AMCP",
            "Aromatic, medicinal and culinary",
          ifelse(
            code == "BW",
            "Buckwheat",
            ifelse(
              code == "CANS",
              "Canary seed",
              ifelse(
                code == "ENG-B",
                "Energy beet",
              ifelse(
                code == "EX-SS",
                "Ex-structural set-aside",
                ifelse(
                  code == "FFS",
                  "Fibre flax",
                  ifelse(
                    code == "GCM",
                    "Green cover mixtures",
                    ifelse(code == "HS", "Hemp",
                           ifelse(
                             code == "MIL",
                             "Millet",
                             ifelse(
                               code == "MU",
                               "Mustard",
                             ifelse(
                               code == "MSC",
                               "Miscanthus",
                               ifelse(
                                 code == "PEM",
                                 "Positive environmental management",
                                 ifelse(code=="PHA",
                                        "Phacelia",
                                 ifelse(
                                   code == "RCG",
                                   "Reed canary grass",
                                   ifelse(code ==
                                            "RYE", "Rye",
                                          ifelse(
                                            code == "SOR",
                                            "Sorghum",
                                            ifelse(
                                              code == "SRC",
                                              "Short rotation coppice",
                                              ifelse(code == "SUN",
                                                     "Sunflower",
                                              ifelse(
                                                code == "TURF",
                                                "Turf production",
                                                ifelse(code ==
                                                         "WBS", "Wild bird seed", 
                                                       ifelse(code ==
                                                                "WFM", "Wild flower mix",code)
                                              )
                                              )
                                            )
                                          )
                                   )
                                 )
                               )
                             )
                           )
                    )
                  )
                )
              )
            )
          )
          )
          )
          )
          )
          )
        ),
        ""
      )
  )
}


ncode<-function(x){
  mutate(x,
    item185 =
      ifelse(cens_code == "item41",
             ifelse(
               item185 != "a", paste(item185, ncode), ncode
             ), ""
      )
  )
}



censusformat<-function(x){
  summarise(x,
    area = sum(area),
    llo_area = sum(llo_area),
    lfass_area = sum(lfass_area),
    mlc = unique(mlc),
    brn = unique(brn)
  )
}


extrafields <-  function(x){
  summarise(x,
  llo_area = sum(llo_area),
  lfass_area = sum(lfass_area),
  mlc = unique(mlc),
  brn = unique(brn)
)
}


brnmutate<-function(x) {
  mutate(x, 
         parish=str_pad(parish, width = 3, pad = "0"),
         holding=str_pad(holding, width = 4, pad = "0"),
         location_type =
           paste0(
             parish, "/",
             holding),
         location_type =
           ifelse(location_type == mlc, "Main", "Subcode"))
}

brnsummary<-function(x) {
  summarise(x,
    area = sum(area),
    mlc = unique(mlc),
    location_type = unique(location_type)
  )
  
}

newitemssaf<-function(x) {
  mutate(x,
    item1 = llo_area,
    item2 = item2879 + item2827 + item2828,
    item27730 = item52 + item53 + item55 + item56 + item2323 + item59 + item60 + item61 +
      item63 + item64 + item65 + item66,
    item76 = item70 + item71 + item72 + item2832 + item75,
    item27740 = item2324 + item1709 + item80 + item81 + item1710 + item82 + item83,
    item68 = item27730,
    item84=item27740,
    item37 = item76,
    item38 = item41 + item27740 + item2858 + item2863 +
      item2859 + item2864 +
      item2860 + item2865 +
      item2861,
    item40 = item14 + item15 + item16 + item18 + item17 + item20 + item3156 +
      item19 + item23 + item21 + item24 + item2320 + item27 + item28 +
      item2034 + item29 + item30 + item31 + item2059 + item32 + item34 + item36 + item2469 + item2470 + item27730 + item37 + item38,
    item46 = item2321 + item2322 + item40,
    item27710=item16+item18,
    item27715=item17+item20,
    item27720=item19+item23,
    item27725=item29+item30+item31+item2059+item32+item34,
    item27735=item36+item70+item71+item72+item2832+item75,
    item2556=item2858+item2863,
    item2557 = item2859+item2864,
    item2836 = item2860+item2865,
    saf_land =
      ifelse((item47 + item49 + item46 + item9999) > 0, 1, "")
  )
}




# Functions for B10 -------------------------------------------------------



#B10 Functions - for merging AS (full and partial datasets) with SAF
#code by Jackie Massaya 01/02/2023

#add land_data and other_data columns to datasets
add_land_other <- function(x)
  
{mutate(x, land_data = "",
        other_data = as.factor("yes"))}




dupsBetweenGroups <- function (df, idcol) {
  # df: the data frame
  # idcol: the column which identifies the group each row belongs to
  
  # Get the data columns to use for finding matches
  datacols <- setdiff(names(df), idcol)
  
  # Sort by idcol, then datacols. Save order so we can undo the sorting later.
  sortorder <- do.call(order, df)
  df <- df[sortorder,]
  
  # Find duplicates within each id group (first copy not marked)
  dupWithin <- duplicated(df)
  
  # With duplicates within each group filtered out, find duplicates between groups. 
  # Need to scan up and down with duplicated() because first copy is not marked.
  dupBetween = rep(NA, nrow(df))
  dupBetween[!dupWithin] <- duplicated(df[!dupWithin,datacols])
  dupBetween[!dupWithin] <- duplicated(df[!dupWithin,datacols], fromLast=TRUE) | dupBetween[!dupWithin]
  
  # ============= Replace NA's with previous non-NA value ==============
  # This is why we sorted earlier - it was necessary to do this part efficiently
  
  # Get indexes of non-NA's
  goodIdx <- !is.na(dupBetween)
  
  # These are the non-NA values from x only
  # Add a leading NA for later use when we index into this vector
  goodVals <- c(NA, dupBetween[goodIdx])
  
  # Fill the indices of the output vector with the indices pulled from
  # these offsets of goodVals. Add 1 to avoid indexing to zero.
  fillIdx <- cumsum(goodIdx)+1
  
  # The original vector, now with gaps filled
  dupBetween <- goodVals[fillIdx]
  
  # Undo the original sort
  dupBetween[sortorder] <- dupBetween
  
  # Return the vector of which entries are duplicated across groups
  return(dupBetween)
}


# Functions for D2 - Imputation ---------------------------------------


create_zeroes<-function(df) {
  df <- df %>% 
    group_by (id) %>% 
    mutate(across(starts_with("item"), ~ifelse(sum(.[yr<=2021], na.rm=TRUE) ==0 & yr == 2023, 0, .))) %>% 
  ungroup()
  return(df)
}



keep_missing<-function(df) {
  df <- df %>% 
    group_by (id) %>% 
    filter_all(any_vars(is.na(.[yr==2023]))) 
  return(df)
}


none_missing<-function(df) {
  df <- df %>% 
    group_by (id) %>% 
    filter((if_all(everything(), ~ !is.na(.[yr==2023]))))
  return(df)
}


saf_filter<-function(df) {
  df <- df %>% 
    group_by (id) %>% 
    filter(!(any(saf_madeup>=10 & yr==2021))) %>% 
    filter_at(vars(starts_with("item")), any_vars(is.na(.[yr==2023]))) #this should result in the same as imptype==full
  
  return(df)
}



ags_filter<-function(df) {
  df <- df %>% 
    group_by (id) %>% 
    filter(!(any(ags_madeup>=10 & yr==2021))) %>% 
  filter_at(vars(starts_with("item")), any_vars(is.na(.[yr==2023]))) #this should result in the same as imptype==Ags
  
  return(df)
}

naLOCFPlusone <- function(x){
  ix <- cumsum(is.na(x))
  zoo::na.locf(x) + ix - cummax(ix * !is.na(x))
}


saf_madeup<-function(df){
  df <- df %>% 
    group_by (id) %>% 
    dplyr::mutate(across(starts_with("saf_madeup"), ~ifelse(is.na(.[yr==2023]) & yr==2023,naLOCFPlusone(.), .))) 
  return(df)
}


roll_forward<-function(df){
  df <- df %>% 
    group_by (id) %>% 
    dplyr::mutate(across(starts_with("item"), ~ifelse(is.na(.[yr==2023]) & yr==2023, zoo::na.locf(., na.rm = FALSE), .))) 
  
  return(df)
}


zero_2023<-function(df){
  df <- df %>% 
    group_by (id) %>% 
    dplyr::mutate(across(starts_with("item"), ~ifelse(is.na(.[yr==2023]) & yr==2023, 0, .))) %>% 
    dplyr::mutate(flag_not_rf=ifelse(yr==2023, 1, 0))
  
  return(df)
}



ags_madeup<-function(df){
  df <- df %>% 
    group_by (id) %>% 
    dplyr::mutate(across(starts_with("ags_madeup"), ~ifelse(is.na(.[yr==2023]) & yr==2023,naLOCFPlusone(.), .)))
  
  return(df)
}



saf_filter_not_rf<-function(df) {
  df <- df %>% 
    group_by (id) %>% 
    filter((any(saf_madeup>=10 & yr==2021))) %>% 
    filter_at(vars(starts_with("item")), any_vars(is.na(.[yr==2023])))
  
  return(df)
}


ags_filter_not_rf<-function(df) {
  df <- df %>% 
    group_by (id) %>% 
    filter((any(ags_madeup>=10 & yr==2021))) %>% 
    filter_at(vars(starts_with("item")), any_vars(is.na(.[yr==2023])))
  
  return(df)
}


zero_nas<-function(df) {
  df <- df %>% 
    dplyr::mutate(across(starts_with("item"), ~ifelse(is.na(.), 0, .)))
  
  return(df)
}

madeup<-function(df) {
  df <- df %>% 
    dplyr::mutate(across(starts_with("madeup"), ~ifelse(yr==2023, pmax(ags_madeup,saf_madeup, na.rm = TRUE), .)))
  
  return(df)
}


# Disaggregation functions ------------------------------------------------


summaryvalues<-function(x) {
mutate(x,
  barley_27710=item16+item18,
  oats_17=item17+item20,
  osr_27720=item19+item23,
  stockfeed_27725=item29+item30+item31+item2059+item32+item34,
  veginopen_27730=item52+item53+item55+item56+item2323+item59+item60+item61+item63+item64+item65+item66,
  fruitinopen_27735=item36+item70+item71+item72+item2832+item75,
  nursery_27740=item2324+item1709+item80+item81+item1710+item82+item83
)
  
}


proportions<-function(x) {
  mutate(x,
    prop_16=item16/barley_27710,
    prop_18=item18/barley_27710,
    prop_17=item17/oats_17,
    prop_20=item20/oats_17,
    prop_19=item19/osr_27720,
    prop_23=item23/osr_27720,
    prop_29=item29/stockfeed_27725,
    prop_30=item30/stockfeed_27725,
    prop_31=item31/stockfeed_27725,
    prop_2059=item2059/stockfeed_27725,
    prop_32=item32/stockfeed_27725,
    prop_34=item34/stockfeed_27725,
    prop_52=item52/veginopen_27730,
    prop_53=item53/veginopen_27730,
    prop_55=item55/veginopen_27730,
    prop_56=item56/veginopen_27730,
    prop_2323=item2323/veginopen_27730,
    prop_59=item59/veginopen_27730,
    prop_60=item60/veginopen_27730,
    prop_61=item61/veginopen_27730,
    prop_63=item63/veginopen_27730,
    prop_64=item64/veginopen_27730,
    prop_65=item65/veginopen_27730,
    prop_66=item66/veginopen_27730,
    prop_36=item36/fruitinopen_27735,
    prop_70=item70/fruitinopen_27735,
    prop_71=item71/fruitinopen_27735,
    prop_72=item72/fruitinopen_27735,
    prop_2832=item2832/fruitinopen_27735,
    prop_75=item75/fruitinopen_27735,
    prop_2324=item2324/nursery_27740,
    prop_1709=item1709/nursery_27740,
    prop_80=item80/nursery_27740,
    prop_81=item81/nursery_27740,
    prop_1710=item1710/nursery_27740,
    prop_82=item82/nursery_27740,
    prop_83=item83/nursery_27740,
  )
  
}
