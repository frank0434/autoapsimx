
#' cat_ParaValue
#' @description A wrapper function of `paste` to construct configuration values
#'   for `ApsimX` Edit feature.
#'
#' @param parameter the address to the parameter that needs to be modified
#' @param values a string of values that are separated by comma
#'
#' @return Character string. The string is the path of apsimx submodule and
#'   value pair
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
