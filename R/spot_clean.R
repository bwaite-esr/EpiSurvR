#' Spot clean a dataset
#'
#' takes a list of corrections
#' and applies them to the dataset
#'
#' @param data your extract
#' @param adjustments a list of corrections. see ?correction
#' @keywords EpiSurv
#' @keywords data cleaning
#' @export
#' @examples
#' # no examples
#' @import dplyr
#' @import magrittr
#' @import rlang
#' 

spot_clean <- function(data,adjustments){
  data <- data %>% mutate(index = row_number())
  for(i in adjustments){
    
    temp <- data %>% filter(eval(i$condition)) 
    index <- temp$index
    adj <- temp %>% mutate(
     !!!eval_tidy(i$change)
    )
    
    data[index,] <- adj
    
  }
  return(data)
}


