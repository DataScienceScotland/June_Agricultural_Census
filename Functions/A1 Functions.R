
# Create line variable to index by LPID. Number gives instance of each LPID. 

LPID_index <- function(x)
  
{setDT(x)[,line:=seq(1,.N),by=LPID]}


# Rename select variables in permanent and seasonal datasets
rename_perm_seas_vars <-  function(x)
  
{rename(x, 
        brn = "Business Reference Number",
        mlc = "Main Location Code",
        slc = "Location Code",
        fid = LPID,
        field_area = "Land Parcel Area",
        lfass_eligible = LFASS,
        eligible_area = "BPS Eligible Area",
        LLO = "Land Leased Out",
        land_use = "Land Use",
        land_use_area = "Land Use Area",
        bps_claimed_area = "BPS Claimed Area"
)}

# Create new variables for permanent and seasonal datasets
new_perm_seas_vars <- function(x)
  
{mutate(x,
        slc = substr(slc,4,11),
        mlc = substr(mlc,4,11),
        parish = str_remove(substr(slc,1,3), "^0+"),
        holding = str_remove(substr(slc,5,8),"^0+"),
        land_use = str_replace_all(land_use, "_", "-"),
        sfp_area = bps_claimed_area,
        sfp_code = land_use,
        other_area = (land_use_area-bps_claimed_area),
        other_code = land_use
)}


# Rename select variables in scheme dataset
rename_scheme_vars <- function(x)
{  rename(x,
  brn = "Business Reference Number",
  mlc = "Main Location Code",
  slc = "Location Code",
  fid = LPID,
)}

# Create new variables for scheme dataset
new_scheme_vars <- function(x)
{
mutate(x,
  slc = substr(slc,4,11),
  mlc = substr(mlc,4,11),
  parish = str_remove(substr(slc,1,3), "^0+"),
  holding = str_remove(substr(slc,5,8),"^0+"),
  SCHEME_OPTION_CODE = str_replace_all(SCHEME_OPTION_CODE, "_", "-") # not sure this line is necessary - there are no underscores or hyphens?
)}

# Find missing observations in permanent and seasonal. 

missing_obs <- function(x)
  
{subset(x,
        is.na(brn) == TRUE | is.na(mlc) == TRUE | is.na(line) == TRUE | is.na(slc) == TRUE | is.na(fid) ==TRUE
)}


# Find missing field areas in permanent and seasonal 
missing_field_area <- function(x)
  
{x[field_area==0 | is.na(field_area)]}


# Find eligible areas in permanent and seasonal
missing_eligible_area <- function(x)
  
{x[is.na(eligible_area)]}


# Remove NAs from datsets

cleaned_datasets <- function(x)
  
{x[!is.na(brn) | !is.na(mlc) | !is.na(slc)]}


