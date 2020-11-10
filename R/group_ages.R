#' Count Yesses in selected columns.
#'
#' This function takes an ag column, breaks and labels,
#' and returns the appropriate age groups

#'
#'
#' @param age_vctr the age column
#' @param age_breaks the breaks
#' @param age_labels the labels

#' @keywords EpiSurv
#' @keywords Age
#' @keywords Age Groups
#' @export
#' @examples
#' # no examples
#' @import stringr
#' @import purrr
#' @import rlang

group_ages <- function(age_vctr,age_breaks,age_labels){
  
  # if the given ages are a character...
  if(str(age_vctr) == "character"){
    
    # and need converting from "## y/m/d" format
    if(any(is.na(as.numeric(age_vctr)))){ 
      
      fract <- map_dbl(
        str_match(age_vctr,"[:lower:]"),
        ~switch(
          .,
          d  = 365,
          m = 12,
          y = 1
          )
        )
      
      numer  <- as.numeric(
        str_match(age_vctr,"[:digit:]*")
        )
      # overwrites passed vector
      age_vctr <- numer/fract
      
    }else{
      # otherwise convert is straight and carry on
      age_vctr <- as.numeric(age_vctr)
    }
  }
  
  # this checks if you're trying to look for e.g. 6 months,
  # but only have whole years in your column
  # isn't necessarily a problem, but best to warn people
  if(is_integer(age_vctr) & any(cut_list %%1 != 0)){
    warning("You're trying to cut to sub-year intervals,\nbut your age column only contains integer years.\nJust a heads up.")
  }
  
  # then a simple cut.
  # I would like to auto-generate the labels at some point.
  return_age_groups <- cut(age_vctr,breaks = age_breaks,labels = age_labels)
  
  return_age_groups
}
