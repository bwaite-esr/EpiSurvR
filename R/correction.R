#' Correction to the dataset
#'
#' a convenience function, to be used within spot_clean
#' passes an individual correction
#'
#' @param given_cond a boolean expression. the conditions to meet e.g. epiSurvNumber == "##-######-XX" etc
#' @param given_edit a named vector of corrections e.g. c(age = "2 yrs",sex = "Male")
#' @keywords EpiSurv
#' @keywords EpiSurv Numbers
#' @export
#' @examples
#' # no examples
#' 
#' @import rlang
#' 

correction <- function(given_cond,given_edit){
  list(
    condition = enquo(given_cond),
    change = enquo(given_edit)
  )
}
