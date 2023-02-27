
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



# Load Rda


loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}


# Main function
.ls.objects <- function (pos = 1, pattern, order.by,
                         decreasing=FALSE, head=FALSE, n=5) {
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.prettysize <- napply(names, function(x) {
    capture.output(format(utils::object.size(x), units = "auto")) })
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
  names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  out
}

# Shorthand function for easy access
lsos <- function(..., n=10) {
  .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}

