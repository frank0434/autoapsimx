
#' cat_ParaValue
#'
#' @param parameter the address to the parameter that needs to be modified
#' @param values a string of values that are separated by comma
#'
#' @return
#' @export
#'
#' @details when replace the number of values must match the number of layers
#' soil parameters.
#'
#'
#' @examples
cat_ParaValue <- function(parameter, values){
  if(!is.na(parameter)
     & is.character(parameter)
     & !is.na(values)
     & is.character(values)){

    parameter_value = paste0(parameter, paste(values,collapse = ","))
    parameter_value
  } else {
    print("Parameter address and values must be strings.")
  }

}
