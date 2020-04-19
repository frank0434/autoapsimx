#' read_met_col
#'
#' @description A helper function to assist read_met.
#' It only read the colnames.
#'
#' @family read_met
#'
#' @param path The path to access the met files.
#' @param skip The lines to skip for reading the header.
#'
#' @return A data .table and .frame is returned, which has only the colnames for met files.
#' @export
#'
#' @examples
#' \dontrun{
#' read_met_col("path", skip = 7)
#' }
read_met_col <- function(path = path_met, skip = 7){
  met_col <- data.table::fread(input = path, skip = skip, nrows = 1)
  met_col
}


#' read_met
#'
#' @param path A character string. The path to access the met files.
#' @param skip_unit An integer. The number of rows for skipping the unit line in met files.
#' @param skip_meta An integer. The number of rows for skipping the meta data before the column names start.
#'
#' @return A data .table and .frame is returned.
#' @export
#'
#' @examples
#' \dontrun{
#' read_met("path", skip_unit = 9, skip_meta = 7)
#' }
read_met <- function(path = path_met, skip_unit = 9, skip_meta = 7){
  met_LN <- data.table::fread(input = path,skip = skip_unit, fill = TRUE)
  met_col <- read_met_col(path = path, skip = skip_meta)
  colnames(met_LN) <- colnames(met_col)
  return(met_LN)
}
