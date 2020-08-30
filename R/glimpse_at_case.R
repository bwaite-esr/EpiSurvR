#' Glimpse at the big picture of the case from EpiSurv
#' I'm checking whether I can push
#' This lets you quickly look at all the fields in a case in your extract
#'
#' @param data The name of the extract.
#' @param case The EpiSurv Number of the case you want to look at. Can be headless.
#' @param in_browser Boolean, false by default. if true, it opens the case report form in EpiSurv.
#' @keywords EpiSurv
#' @keywords cases
#' @export
#' @examples
#' @import dplyr
#' @import tibble
#' no examples
#'


glimpse_at_case <- function(data,case,in_browser = FALSE){
  case_data <- filter(data,str_detect(EpiSurvNumber,case))
  if(in_browser){
    browseURL(
      url =
        paste(
          "https://episurv.survinz.esr.cri.nz/EpiSurvCaseReport.aspx?EpiSurvNo=",
          case_data[["EpiSurvNumber"]],
          sep=""
          ),
      browser = NULL)
  }else{
  return(glimpse(case_data))
  }
}
