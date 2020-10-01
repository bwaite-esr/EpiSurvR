#' Count Yesses in selected columns.
#'
#' This function takes selected columns and checks if Yes is in each row.
#' Unfortunately at the moment it needs the first argument "." if used within mutate
#'
#'
#' @param data your extract
#' @param ... variables you want checked.
#' @param operation a string - one of the following: 
#' "any" returns TRUE if at least 1 Yes present.
#' "all" returns TRUE only if all columns are Yes
#' "n" returns the number of Yesses in the row.
#' @keywords EpiSurv
#' @keywords EpiSurv Numbers
#' @export
#' @examples
#' # no examples
#' @import dplyr
#' @import magrittr
#' @import tibble
#' 

is_yes <- function(...,operation = "any"){
  
  data <- tibble(...)
  data <- data %>% 
    select(...) %>% 
    mutate_all(~recode(.,"Yes"=1,.default=0)) %>% 
    mutate(row_count = rowSums(.,na.rm = TRUE)) %>% 
    mutate(any = row_count > 0,
           all = row_count == length(data),
           n = row_count)
  return(data[[operation]])
}
