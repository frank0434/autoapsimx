

#' subset_stats
#' @description Unlist the stats result to obtain best fit for each layer and
#'   join back to the data for further visulisation
#' @param DT A data.table with two nested list columns: `data` and `stats`.
#'   Column names `Source, Experiment, SowingDate, Depth` must be in the DT as
#'   group variables
#' @import data.table
#' @return a data.table has the best fit parameter for each layer in each
#'   treatment.
#' @export
#'
#' @examples
#' subset_stats(DT = DT_stats_layerKL)
subset_stats <- function(DT = DT_stats_layerKL){
  stats = DT[, Source := basename(Source)
             ][order(Experiment, SowingDate, Depth)]
  top3 = stats[, unlist(stats, recurse = FALSE), by = .(Source, Experiment, SowingDate, Depth)
               ][Depth != 1
                 # & NSE > 0
                 ][order(NSE,R2, RMSE, decreasing = TRUE),
                   index := seq_len(.N),
                   by = list(Experiment, SowingDate, Depth)
                   ]

  JOINNED = stats[top3[index == 1], on = c("Source", "Experiment","SowingDate","Depth")
                  ][, unlist(data, recurse = FALSE),
                    by = .(Source, Experiment, SowingDate, Depth)
                    ][, kl := regmatches(Source, regexpr("kl0\\.\\d{1,3}", Source, perl = TRUE))]

  id = JOINNED$Source %>% unique()
  top1stats = stats[Source %in% id
                    ][, unlist(stats, recurse = FALSE),
                      by = .(Source, Experiment, SowingDate, Depth)]
  JOINNED_stats = top1stats[JOINNED, on = "Source"]
  JOINNED_stats[, ':='(NSE = paste0("NSE=\r\n",NSE),
                       R2 = paste0("R2=\r\n", R2),
                       RMSE = paste0("RMSE=\r\n", RMSE))]
  JOINNED_stats
}
#' subsetByTreatment
#' @description Subset the observation/prediction data based on treatment.
#'   Current version can only do two treatment subset.
#'
#' @param DT Data.table. The data.table contains observation or prediction
#'   values for all treatments.
#' @param col_treatment1 Character string. The column name for treatment 1
#' @param col_treatment2 Character string. The column name for treatment 2
#' @param treatment1 The treatment value that wants to be subset.
#' @param treatment2 The treatment value that wants to be subset.
#' @param mode Default NULL is subset
#'
#' @return A filtered data.table.
#'
#' @export
#'
#' @examples
#' subsetByTreatment(DT = SW_mean,
#'                   col_treatment1 = "Experiment",
#'                   col_treatment2 = "SowingDate",
#'                   treatment1 = "Iversen12",
#'                   treatment2 = "SD1")
subsetByTreatment <-  function(DT = SW_mean, mode = NULL,
                               col_treatment1 = "Experiment",
                               col_treatment2 = "SowingDate",
                               treatment1 = "Iversen12",
                               treatment2 = "SD1"){

  if(isTRUE(mode == "prediction") && c("SKL", "KLR", "RFV") %in% colnames(DT)){
    DT = DT[, KLR_RFV_SKL := paste0("klReduction=", KLR,
                                    "\r\n", "RFV=", RFV,
                                    "\r\n", "SKL=", SKL)
            ][get(col_treatment1) == treatment1 & get(col_treatment2) == treatment2]
    DT
  } else if(isTRUE(mode == "prediction") | isTRUE(mode == "observation") | is.null(mode)){

    DT = DT[get(col_treatment1) == treatment1 & get(col_treatment2) == treatment2]
    DT
  }

}


#' plot_params
#'
#' @description
#'
#' @param DT data.table that has predictio and observation data with stats
#'   resutls
#' @param title Charater string - Path with file name to save the figure
#' @param col_pred Charater string - column name for prediction results
#' @param col_obs Charater string - column name for observation results
#' @param point_size Integer - the size of the point
#' @param Depth NULL or charater string to control the facet
#' \itemize{
#'   \item Default is Null - water profile.
#'   \item "Depth" to check each layer.
#'   \item "Layerkl_distribution" to check the overall distribution of best fit
#'   kls in each layer
#'   \item "Layerkl" to check the best fit for each layer with Statistics results
#'   }
#' @param format Charater string to indicate output figure format.
#' @param width Integer. The width of the figure in inches.
#' @param height Integer. The height of the figure in inches.
#' @param group_params Charater strings. The strings are column names which will
#'   be used as facet factor
#' @param stats TRUE or FALSE. Default is TRUE where NSE r.square and RMSE must
#'   be in the data.table
#'
#' @import ggplot2
#' @import stringr
#'
#' @return No values return to the console. Figures will be output directly into
#'   the designated folder
#' @export
#'
#' @examples
#' plot_params(DT = top5, title, format = "pdf", col_pred = "PSWC", col_obs =
#' "SWC",point_size = 2,Depth = NULL,width = 10, height = 10)
#'
plot_params <- function(DT = top5,
                        title, format = "pdf",
                        col_pred = "PSWC",
                        col_obs = "SWC",
                        group_params = "KLR_RFV_SKL",
                        stats = TRUE,
                        point_size = 2,
                        Depth = NULL,
                        width = 10, height = 10){
  palette = rep("grey", times = 2)
  palette_named = setNames(palette,  c(col_pred, col_obs))
  palette_named[2] = "red"



  if(is.null(dim(DT)) | isTRUE(nrow(DT) == 0)){
    print("The data table is empty!!!")
  } else if(! col_pred %in% colnames(DT)){
    cat(col_pred, "is not in the data.table! \r\n")
  } else if(! col_obs %in% colnames(DT)){
    cat( "is not in the data table! \r\n")
  } else {

     p = DT %>%
       ggplot(aes(Clock.Today)) +
       geom_line(aes(y = get(col_pred), color = names(palette_named)[1]), size = point_size) +
       geom_point(aes(y = get(col_obs), color = names(palette_named)[2]), size = point_size) +
       ggtitle(paste0(unique(DT$Experiment), unique(DT$SowingDate))) +
       scale_x_date(date_labels = "%Y %b", date_breaks = "4 weeks") +
       scale_color_manual(name = "", values = palette_named) +
       theme_water() +
       theme(legend.position = "top",
             axis.text.x = element_text(angle = 30, hjust = 1))

  if(is.null(Depth)){
    facet_form =  reformulate(".", group_params )
    if(isTRUE(stats)){
      p +
        geom_text(aes(x = as.Date("2011-07-01"), y = median(DT[[col_pred]]),
                      label = paste0("NSE = ", NSE,
                                     "\r\nR.square = ", R2,
                                     "\r\nRMSE = ", RMSE)),inherit.aes = FALSE)+
        facet_grid( facet_form) +
        ggsave(stringr::str_glue("{title}.{format}"),
               width = width, height = height, dpi = 300)
    } else {
      p +
        facet_grid( facet_form) +
        ggsave(stringr::str_glue("{title}.{format}"),
               width = width, height = height, dpi = 300)
    }

  } else if(Depth == "Depth"){
    facet_form =  reformulate(group_params, "Depth" )
    if(isTRUE(stats)){
      p +
        geom_text(aes(x = as.Date("2011-07-01"), y = median(DT[[col_pred]]),
                      label = paste0("NSE = ", NSE,
                                     "\r\nR.square = ", R2,
                                     "\r\nRMSE = ", RMSE)),inherit.aes = FALSE)+
        facet_grid( facet_form) +
        ggsave(stringr::str_glue("{title}.{format}"),
               width = width, height = height, dpi = 300)
    } else {
      p +
        facet_grid( facet_form) +
        ggsave(stringr::str_glue("{title}.{format}"),
               width = width, height = height, dpi = 300)
    }
  } else if(Depth == "Layerkl_distribution"){
    facet_form =  reformulate("kl", "Depth" )
    p +
      facet_grid( facet_form) +
      ggsave(stringr::str_glue("{title}.{format}"),
             width = width, height = height, dpi = 300)
  } else if(Depth == "Layerkl"){
    facet_form =  reformulate(".", "Depth + kl + NSE + RMSE + R2" )
    p +
      facet_grid( facet_form) +
      ggsave(stringr::str_glue("{title}.{format}"),
             width = width, height = height, dpi = 300)
  }
  }
}


#' Title
#'
#' @param DT data.table that has predictio and observation data with stats
#' @param title
#' @param format
#' @param col_pred
#' @param col_obs
#' @param point_size
#' @param Depth
#' @param width
#' @param height
#'
#' @return
#' @export
#'
#' @examples
plot_root <- function(DT = top1,
                      title, format = "pdf",
                      col_pred = "PSWC",
                      col_obs = "SWC",
                      point_size = 2,
                      Depth = NULL,
                      width = 10, height = 10){
  if(is.null(dim(DT)) | isTRUE(nrow(DT) == 0)){
    print("The data table is empty!!!")
  } else if(! col_pred %in% colnames(DT)){
    cat(col_pred, "is not in the data.table! \r\n")
  } else if(! col_obs %in% colnames(DT)){
    cat( "is not in the data table! \r\n")
  } else {
  cf = 2300/max(DT$PSWC)
  col_pred = "PSWC"
  palette = rep("black", times = 3)
  palette_named = setNames(palette,  c("PSWC", "RootDepth","SWC"))
  palette_named[2:3] = c("blue", "red")
  shapes = c(95, 124, 16)
  shapes_named = setNames(shapes, c("PSWC", "RootDepth","SWC"))
  DT[,.(Clock.Today, PSWC,SWC, RootDepth)] %>%
    ggplot(aes(Clock.Today)) +
    geom_line(aes(y = PSWC), size = 1, show.legend = NA) +
    geom_point(aes(y = SWC, color = "SWC", shape = "SWC"), show.legend = NA) +
    theme_water() +
    scale_x_date(date_labels = "%Y %b", date_breaks = "8 weeks") +
    geom_point(aes(y=RootDepth/cf, color="RootDepth", shape = 'RootDepth'),
               size = 5,show.legend = NA)+
    scale_y_continuous(limits = range(DT$PSWC),
                       sec.axis = sec_axis(~ . *cf, name = "Root Depth")) +
    scale_shape_manual(name = "",values = shapes_named)+
    theme(legend.position = c(0.1,0.1)) +
    scale_color_manual(name = "",values = palette_named) +
    ggsave(stringr::str_glue("{title}.{format}"),
           width = width, height = height, dpi = 300)
  }

}
