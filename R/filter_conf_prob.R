#' Filter confirmed and probable cases in an extract
#'
#' I know this being criminally lazy, but it feels right to make this function
#'
#'
#' @param data your extract

#' @keywords EpiSurv
#' @keywords Status
#' @export
#' @examples
#' # no examples
#' @import dplyr
#' @import magrittr
#' 

filter_conf_prob <- function(data) {
  
  status_caps <- "Status" %in% colnames(data)
  
  if(status_caps){
    data %>% filter(Status %in% c("Confirmed","Probable"))
  }
  else{data %>% filter(status %in% c("Confirmed","Probable"))}
}