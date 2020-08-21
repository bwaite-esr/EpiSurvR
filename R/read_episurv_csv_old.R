#' Read an EpiSurv csv
#'
#' This function wraps read.csv() for reading in EpiSurv csv files, which are very fiddly.
#'
#' @param file the name of the extract.
#' @param ... other arguments to pass to read.csv
#' @keywords EpiSurv
#' @keywords csv
#' @export
#' @examples
#' # no examples
#'



read_episurv_csv <- function(file,...){
  return(
    read.csv(file = file,fileEncoding = "UTF-16",stringsAsFactors = FALSE,strip.white = TRUE,...)
  )
}

