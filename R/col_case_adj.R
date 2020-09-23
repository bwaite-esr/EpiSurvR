#' adjust variable names
#'
#' at the moment episurv changes the variable names to camelCase.
#' to make it easy to sync up with various scripts, 
#' this function adjusts the variable names
#' to upper or lower to suit your needs
#'
#'
#' @param data your extract

#' @keywords EpiSurv
#' @keywords colnames
#' @export
#' @examples
#' # no examples
#' 

col_case_adj <- function(data,to_case = "upper"){
  
  case_change <- switch(to_case,"upper"=toupper,"lower"=tolower)
  oldnames <- colnames(data)
  
  colnames(data) <- paste(
    case_change(substr(oldnames,1,1)),
    substr(oldnames,2,1000),
    sep="")
  data
}
