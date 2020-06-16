# A collection of 3 functions to achieve the following purposes:
# 1. Extract the treatment information from db file name
# 2. Read in the db file
# 3. Subset the observation data by treatment and join with the appropioate
# simulation data
# 4. Nested the joined data
# 5. Do stats via hydroGOF::gof
# 6. Unnested the data and output

#' extract_trts
#' @description extract treatment information from output db names by using
#'   regular expression. It currently only works for two treatments.
#'
#' @param filename a string or a full path of the db name
#' @param pattern a regex pattern to remove extra strings other than treatment
#'   information
#'
#' @return a string with two elements. first is the site and the second is the
#'   sowing date
#' @export
#'
#' @examples
extract_trts <- function(filename,
                         pattern = ".+ModifiedSKL_0\\.\\d{1,2}|\\.db",
                         pattern_split = "(.+)(SD\\d{1,2})"){
  site_sd <- gsub(pattern,"",filename)
  site_sd <- gsub(pattern_split, "\\1_\\2", site_sd)
  site_sd <- unlist(strsplit(site_sd, split = "_"))
  site_sd
}


#' read_dbtab
#'
#' @description Connet to `sqlite3` file and read a specific table and convert
#'   the date string to a date type
#'
#' @param path the path to the sqlite file
#' @param table a charater string for the table name
#'
#' @return
#' @export
#' @import data.table
#' @examples
read_dbtab <- function(path = "./03processed-data/Richard.sqlite3", table = "SoilWater"){
  # Build the connection
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  dt <- DBI::dbReadTable(con, name = table, check.names = FALSE)

  dbDisconnect(con)
  if("Clock.Today" %in% colnames(dt)){
    # Update the charater string to Date
    dt$Clock.Today <- as.Date(dt$Clock.Today)
  } else if("Date" %in% colnames(dt)){
    dt$Date <- as.Date(dt$Date)
  }
  dt <- data.table::as.data.table(dt)

  return(dt)
}

#' manipulate
#' @description Take a subset data.table subsetted by the treament and join it
#'   with the corresponding simulation results
#'
#' @param DT_obs a data table that subsetted from the aggregated raw data
#' @param DT_pred a data table that has the simulation results
#'
#' @return a data table has the same number of rows as the observation
#'   data.table
#' @export
#' @import data.table
#' @examples
manipulate <- function(DT_obs = obs, DT_pred = dt){
  if(data.table::is.data.table(DT_obs) &
     data.table::is.data.table(DT_pred)){
    pred_swc <- DT_pred[,list(Date, SimulationID,KLR, RFV, SKL, k, LAI, PSWC)
                        ][order(SimulationID)]
    pred_obs <- pred_swc[DT_obs, on = c("Date" = "Clock.Today" )
                         ][order(SimulationID)]
  }
  return(pred_obs)
}

#' sims_stats
#' @description Process one kl set with six KLR and six RFV
#'
#' @param pred_obs a data.table which merged from the simulation table and
#'   observation data.table
#' @param keys grouping variables
#' @param col_pred the colname for prediction result
#' @param col_obs the colname for observation
#'
#' @import data.table
#' @import hydroGOF
#'
#' @return
#' @export
#'
#' @examples
sims_stats <- function(pred_obs,
                       keys = c("Experiment", "SimulationID", "SowingDate",
                                "KLR","RFV","SKL"),
                       col_pred = "PSWC",
                       col_obs = "SWC"
                       ){
  if(data.table::is.data.table(pred_obs)){

    stopifnot(exprs = {
      col_pred %in% colnames(pred_obs)
      col_obs %in% colnames(pred_obs)
    })

    setkeyv(pred_obs, keys)
    nested <- pred_obs[, list(data = list(.SD)), by = key(pred_obs)]

    # 5 calcualting inside the nested
    nested <- nested[, stats := lapply(data, function(x){
      # Calculate the stats via goodness of fit
      m <- hydroGOF::gof(x[[col_pred]], x[[col_obs]])
      # Convert the matrix into a data.table with colnames
      m <- m %>%
        as.data.table(keep.rownames = T) %>%
        transpose(make.names = 'rn')
    })]
    # 6 Unnest the data.table
    # stats <- nested[, unlist(stats, recursive = FALSE), by = key(pred_obs)]
    nested
  }

}


#' sims_stats_multi
#' @description assemble the function above to process multiple dbs
#' @param path_sims
#' @param pattern db extension
#' @param DT_observation
#'
#' @import data.table
#'
#' @return
#' @export
#'
#' @examples
sims_stats_multi <- function(path_sims, pattern = ".db$", DT_observation){
  # Set up
  t1 <- Sys.time()
  dbs <- list.files(path = path_sims, pattern = pattern, full.names = TRUE)
  # A list to store all the stats results
  l_stats <- vector("list", length = length(dbs))
  names(l_stats) <- dbs
  no <- 1L
  for (i in dbs) {
    # 1 read db
    dt <- read_dbtab(path = i, table = "Report")
    cat("Processing", i, no,"of", length(dbs), ".\r\n")
    no = no + 1L
    site <- extract_trts(filename = i)[1]
    sd <- extract_trts(filename = i)[2]
    obs_sd <- data.table::setDT(DT_observation)[Experiment == site & SowingDate == sd]

    pred_swc <- manipulate(DT_obs = obs_sd, DT_pred = dt)
    stats <- sims_stats(pred_swc)
    l_stats[[i]] <- stats
  }

  t2 <- Sys.time()

  return(l_stats)
  return(t2-t1)

}
