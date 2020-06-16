#' DUL_LL
#' @description
#' @param x a numeric vector. Soil water
#' @param thickness
#'
#' @return
#' @export
#'

DUL_LL <- function(x, thickness){
  try(if(!is.numeric(x)) stop("x must be numeric."))
  list(max = max(x, na.rm = TRUE)/swcTovwc,
       min = min(x, na.rm = TRUE)/swcTovwc)
  }
