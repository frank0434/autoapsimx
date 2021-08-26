#' group_in_season
#' @description  Group annual data into seasonal data 1 July to 30 June next year
#'
#'
#' @param DT, data.table
#'
#' @return data.table with a column `Season`
#' @export
#' @details the data.table must have a column named as "Date"
#'
#' @examples
group_in_season <- function(DT){

  stopifnot("Date" %in% colnames(DT))

  period <- range(DT$Date)
  noofyear <- diff.Date(period, unit = "year") %>%
    as.numeric(.)/365
  startyear <- data.table::year(period[1])
  endyear <- data.table::year(period[2])
  startmd <- "-07-01"
  endmd <- "-06-30"
  noofseason <- round(noofyear, digits = 0)

  # Initial a vector to store the text as cmd
  cmd <- vector("character", noofseason)

  # Build a cmd to do conditional evaluation

  for(i in 0:(noofseason)){
    # Key condition
    v <- paste0("Date >= \"" , startyear + i, startmd,"\"","&",
                "Date <= \"", startyear + i + 1, endmd, "\"",",",
                "\"", startyear +i,"/", startyear + i + 1, "\"",",")
    # Check the format
    # cat(v)
    # Store it; must be i + 1 since R has no 0 position
    cmd[i + 1] <- v
  }
  # Collapse into one string and glue the fcase function
  cmd <- paste0("fcase( ", paste(cmd, collapse = ""), ")")

  # Delete the end comma
  cmd <- gsub(",)$", ")", cmd)
  # Check format again
  cat("Check if the command format is correct\r\n", cmd)

  DT[, Season:= eval(parse(text = cmd))]
  return(DT)


}
