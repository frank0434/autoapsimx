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
#' @param startd
#' @param endd
#' @param site
#'
#' @return A data .table and .frame is returned.
#'
#' @import data.table
#' @export
#'
#' @examples
#' \dontrun{
#' read_met("path", skip_unit = 9, skip_meta = 7)
#' }
read_met <- function(path = path_met, skip_unit = 9, skip_meta = 7,
                     startd = "2010-10-01", endd = "2012-08-01",
                     site = "AshleyDene"){
  start_date <- as.Date(startd)
  end_date <- as.Date(endd)
  met_LN <- data.table::fread(input = path,skip = skip_unit, fill = TRUE)
  met_col <- read_met_col(path = path, skip = skip_meta)
  colnames(met_LN) <- colnames(met_col)

  met_LN[, Clock.Today := as.Date(day, origin = paste0(year, "-01-01"))
         ][Clock.Today > start_date & Clock.Today < end_date
           ][, AccumTT := cumsum(mean)
             ][, Experiment:=site]

  return(met_LN)
}
