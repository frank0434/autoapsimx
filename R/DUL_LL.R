#' DUL_LL
#'
#' @param x
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
