
# General functions -------------------------------------------------------


# Load Rda with a name


loadRData <- function(fileName) {
  # loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}


# The following two functions are for looking at how much memory is taken up by each object in the environment

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


# flatten function

flatten <- function(data) {
  ListCols <- sapply(data, is.list)
  cbind(data[!ListCols], t(apply(data[ListCols], 1, unlist)))
}
# Functions used in A -----------------------------------------------------


# Create line variable to index by LPID. Number gives instance of each LPID.

LPID_index <- function(x) {
  setDT(x)[, line := seq(1, .N), by = lpid]
}


# Rename select variables in permanent and seasonal datasets
rename_perm_seas_vars <- function(x) {
  dplyr::rename(x,
    brn = "business_reference_number",
    mlc = "main_location_code",
    slc = "location_code",
    fid = lpid,
    field_area = "land_parcel_area",
    lfass_eligible = lfass,
    eligible_area = "bps_eligible_area",
    LLO = "land_leased_out",
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
    parish = str_remove(substr(slc, 1, 3), "^0+"),
    holding = str_remove(substr(slc, 5, 8), "^0+"),
    land_use = str_replace_all(land_use, "_", "-"),
    sfp_area = bps_claimed_area,
    sfp_code = land_use,
    other_area = (land_use_area - bps_claimed_area),
    other_code = land_use
  )
}


# Rename select variables in scheme dataset
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


# Find eligible areas in permanent and seasonal
missing_eligible_area <- function(x) {
  x[is.na(eligible_area)]
}


# Remove NAs from datasets

cleaned_datasets <- function(x) {
  x[!is.na(brn) | !is.na(mlc) | !is.na(slc)]
}




# Functions for B --------------------------------------------------------

# B6

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
           recode(as.factor(sfp_code), "TREE" = "TREES", "UNSP" = "EXCL"),
         sfp_code =
           recode(as.factor(other_code), "TREE" = "TREES", "UNSP" = "EXCL")
  )
}

# create new variables

newvarsother<-function(x)  {
  mutate(x,
        claimtype = "OTHER",
        other_code=as.character(other_code),
        prefix = substr(other_code, 1, 4),
        code = other_code,
        crops = other_code,
        area = other_area,
        LLO =
          ifelse(prefix == "LLO-", "Y", ifelse(other_code == "LLO", "Y", LLO)),
        other_code =
          ifelse(prefix == "LLO-", substring(other_code, 5), other_code),
        other_code =
          ifelse(other_code == "", "OTH-LAND", other_code)
)
}


newvarssfp<-function(x)  {
  mutate(x,
         sfp_code = as.character(sfp_code),
         claimtype = "SFPS",
         prefix = substr(sfp_code, 1, 4),
         code = sfp_code,
         crops = sfp_code,
         area = sfp_area,
         LLO =
           ifelse(prefix == "LLO-", "1", ifelse(sfp_code == "LLO", "Y", LLO)),
         sfp_code =
           ifelse(prefix == "LLO-", substring(sfp_code, 5), sfp_code),
         sfp_code =
           ifelse(sfp_code == "", "OTH-LAND", sfp_code),
         line =
           ifelse(other_area > 0, line + 0.01, line)
  )
}


# create parish and holding from slc


parishholdingslc<-function(x)  {
mutate(x,
  parish = str_remove(substr(slc, 1, 3), "^0+"),
  holding = str_remove(substr(slc, 5, 8), "^0+"),
)
}


# Seasonal



newvarsseas<-function(x)  {
  mutate(x,
  claimtype = "SFPS",
  LLO = "N",
  code = sfp_code,
  crops = sfp_code,
  area = sfp_area,
  slc = ifelse(slc == "", mlc, slc)
)
}

# create parish and holding from slc


parishholdingmlc<-function(x)  {
  mutate(x,
         parish = str_remove(substr(mlc, 1, 3), "^0+"),
         holding = str_remove(substr(mlc, 5, 8), "^0+"),
  )
}



#B7

fid_index   <-  function(x) {
  setDT(x)[, line := seq(1, .N), by = fid]
}


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




