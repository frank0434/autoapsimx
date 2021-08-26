#' DUL_LL
#'
#' @param SW data.table. Mean soil water content measurments with `Experiment`,
#' `SowingDate` and `Depth` as three key columns.
#' @param id.vars
#' @param startd
#' @param endd
#'
#' @description A wrapper function to be used in data.table syntax with lappy.
#'   `DUL_LL` function will find the maximum and minimum water content (mm) in a
#'   given layer of soil and return its volumetric water content.
#'
#'
#' @return Max and Min VWC
#' @export
#'

doDUL_LL_range <- function(SW, id.vars = id_vars,
                           startd = "2011-01-01", endd = "2012-06-30") {

  SW <- SW[Clock.Today %between% c(as.Date(startd), as.Date(endd))]
  # should only choose first 5 sowing dates for this
  needed <- grep("VWC", colnames(SW), value = TRUE)
  needed <- c(id.vars, needed)
  VWC <- SW[,..needed]

  Dates_max <- filter_datemax(mean_SW = VWC, id.vars = id.vars, mode = "max")
  Dates_min <- filter_datemax(mean_SW = VWC, id.vars = id.vars, mode = "min")

  DT <- data.table::melt.data.table(VWC, id.vars = id.vars,
                                    # measure.vars = value.vars,
                                    variable.name = "Depth",
                                    variable.factor = FALSE,
                                    value.name = "SW")

  DUL_range <- DT[setDT(Dates_max), on = c("Experiment", "SowingDate", "Depth",
                                           "Clock.Today")]
  LL_range <- DT[setDT(Dates_min), on = c("Experiment", "SowingDate", "Depth",
                                          "Clock.Today")]
  ranges <- merge.data.table(DUL_range, LL_range,
                             by = c("Experiment", "SowingDate", "Depth"),
                             suffixes = c(".DUL", ".LL"))
  ranges <- ranges[, Depth := as.integer(gsub("\\D", "", Depth))
  ][order(Experiment, SowingDate, Depth)]
  return(ranges)

}

#' filter_datemax
#' @description need to know which date has the maximum and minimum SW to figure
#'   out the range from the replicates
#'
#' @param mean_SW data.table has mean soil water measurements.
#' @param mode character string to indicate which mode to filter: "max" or "min"
#' @param id.vars a character vector indicates the grouping variables
#'
#' @return
#' @import dplyr
#'
#' @examples
filter_datemax <- function(mean_SW, mode = c("max", "min"), id.vars = id_vars){
  if(mode == "max"){
    TEST <- data.table::melt.data.table(mean_SW, id.vars = id.vars,
                                        variable.name = "Depth",
                                        variable.factor = FALSE,
                                        value.name = "SW") %>%
      dplyr::group_by(Experiment, SowingDate, Depth) %>%
      dplyr::filter(SW == max(SW)) %>%
      dplyr::group_by(Experiment, SowingDate, Depth, SW) %>%
      dplyr::filter(Clock.Today == first(Clock.Today))
  } else if(mode == "min"){
    TEST <- data.table::melt.data.table(mean_SW, id.vars = id.vars,
                                        variable.name = "Depth",
                                        variable.factor = FALSE,
                                        value.name = "SW") %>%
      dplyr::group_by(Experiment, SowingDate, Depth) %>%
      dplyr::filter(SW == min(SW)) %>%
      dplyr::group_by(Experiment, SowingDate, Depth,SW) %>%
      dplyr::filter(Clock.Today == first(Clock.Today))
  }


  return(TEST)
}
