#' Trim EpiSurv Numbers
#'
#' You can pass your episurv numbers to this and it returns them trimmed i.e.
#' the suffix (-AK) is removed.
#'
#' @param data your extract
#' @param ... variables you want trimmed.
#' @keywords EpiSurv
#' @keywords EpiSurv Numbers
#' @export
#' @examples
#' # no examples
#' @import dplyr
#' @import stringr
#' @import magrittr



trim_episurv_numbers <- function(data,...) {

  if(
    any(
      !str_detect(
        stack(select(data,...))$values,
        "[0-9]{2}-[0-9]{6}-[A-Z]{2}"
        )
      )
    ){
    warning("Something in your variables doesn't look like an EpiSurvNumber.",
            " This may cause unexpected outputs.",call. = FALSE)
  }

  data %>%
    mutate_at(vars(...),~substr(.,1,9))



}
