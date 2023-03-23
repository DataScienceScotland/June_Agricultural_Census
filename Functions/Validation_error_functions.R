
## punch error checks function for selecting columns specific to error 
keep_perr_vars <- function(x){
  perr_x <<- errors %>% select(parish, holding, survtype, all_of(x))    
  name_x <<- sub("\\_.*", "", substitute(x))
}