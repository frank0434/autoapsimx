#' DUL_LL
#' @description A wrapper function to be used in data.table syntax with lappy.
#'   `DUL_LL` function will find the maximum and minimum water content (mm) in a
#'   given layer of soil and return its volumn metric content.
#'
#'
#' @param x A numeric vector. Soil water
#' @param thickness Integer. The depth of a given soil layer in mm.
#'
#' @return Max and Min VWC
#' @export
#'

DUL_LL <- function(x, thickness){
  try(if(!is.numeric(x)) stop("x must be numeric."))
  list(max = max(x, na.rm = TRUE)/swcTovwc,
       min = min(x, na.rm = TRUE)/swcTovwc)
  }
