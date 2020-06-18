
#' subset_obs
#' @description subset the obersvation data based on treatment
#'
#' @param DT
#' @param col_treatment1
#' @param col_treatment2
#' @param treatment1
#' @param treatment2
#'
#' @return
#' @export
#'
#' @examples
subset_obs <-  function(DT = SW_mean,
                        col_treatment1 = "Experiment",
                        col_treatment2 = "SowingDate",
                        treatment1, treatment2){
  DT = DT[get(col_treatment1) == treatment1 & get(col_treatment2) == treatment2]
  DT
}
#' subset_pred
#'
#' @param DT
#' @param col_treatment1
#' @param col_treatment2
#' @param treatment1
#' @param treatment2
#'
#' @import data.table
#' @return
#' @export
#'
#' @examples
subset_pred <- function(DT = top5,
                          col_treatment1 = "Experiment",
                          col_treatment2 = "SowingDate",
                          treatment1, treatment2){
  DT = DT[, KLR_RFV_SKL := paste0("klReduction=", KLR,
                                  "\r\n", "RFV=", RFV,
                                  "\r\n", "SKL=", SKL)
          ][get(col_treatment1) == treatment1 & get(col_treatment2) == treatment2]
  DT
}

#' plot_params
#'
#' @description
#'
#' @param DT
#' @param title
#' @param col_pred
#' @param col_obs
#' @param point_size
#' @param Depth default is profile. can be set the "Depth" to check each layer.
#'
#' @import ggplot2
#' @import stringr
#'
#' @return
#' @export
#'
#' @examples
plot_params <- function(DT = top5,
                        title,
                        col_pred = "PSWC",
                        col_obs = "SWC",
                        point_size = 2,
                        Depth = NULL,
                        width = 10, height = 10){
  palette = rep("grey", times = 2)
  palette_named = setNames(palette,  c(col_pred, col_obs))
  palette_named[2] = "red"
  if(is.null(Depth)){
    facet_form =  reformulate(".", "KLR_RFV_SKL" )
  } else if(Depth == "Depth"){
    facet_form =  reformulate("KLR_RFV_SKL", "Depth" )

  }
  if(is.null(dim(DT)) | nrow(DT) == 0){
    print(paste0(DT, " is empty!!!"))
  } else {

      DT %>%
        ggplot(aes(Date)) +
        geom_point(aes(y = get(col_pred), color = names(palette_named)[1]), size = point_size) +
        geom_point(aes(y = get(col_obs), color = names(palette_named)[2]), size = point_size) +
        geom_text(aes(x = as.Date("2011-07-01"), y = median(DT[[col_pred]]),
                      label = paste0("NSE = ", NSE,
                                     "\r\nR.square = ", R2,
                                     "\r\nRMSE = ", RMSE)),inherit.aes = FALSE)+
        facet_grid( facet_form) +
        ggtitle(paste0(unique(DT$Experiment), unique(DT$SowingDate))) +
        scale_x_date(date_labels = "%Y %b", date_breaks = "4 weeks") +
        scale_color_manual(name = "", values = palette_named) +
        theme_water() +
        theme(legend.position = "top",
              axis.text.x = element_text(angle = 30, hjust = 1)) +
        ggsave(stringr::str_glue("{title}.pdf"), width = width, height = height, dpi = 300)
    }

}
