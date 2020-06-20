#' EditLayerKL
#' @description Edit a base `.apsimx` file with a _kl_ value to update a
#'   specific layer in the soil model
#'
#' @param layer an integer for which layer needs to be modified
#' @param value a floating point number. must be less than 1. this value should
#'   be sesenable biologically
#' @param path the path to apsimx `Models.exe`
#' @param saveTo a path to show where to save modified file. This is for
#'   preventing the **Edit** flag overwriten the base `.apsimx` file.
#' @param apsimx the path to the base `.apsimx` file
#'
#' @return
#' @export
#'
#' @examples
EditLayerKL <- function(layer, value, path, apsimx, saveTo){
  try(if(!dir.exists(saveTo)) stop("Directory to save is not there!",
                                   call. = FALSE))
  origName <- gsub("Modified|.apsimx$", "", basename(apsimx))

  modifiedName <- file.path(saveTo, paste0(origName, "L", layer, "kl", value, ".apsimx"))
  layer <- as.integer(layer)
  value <- as.double(value)
  if(is.integer(layer) && is.double(value) && value < 1){
    kl_node <- "[Soil].Physical.SlurpSoil.KL"
    kl_layer <- paste0(kl_node, "[", layer, "] = ", value)
    config_kl <- tempfile('config')
    f <- file(config_kl, "w")
    cat(kl_layer, "\r\n",  file = config_kl)

    close(f)

    system(paste("cp", apsimx, modifiedName))
    system(paste(path, modifiedName, '/Edit', config_kl))



  }

}
