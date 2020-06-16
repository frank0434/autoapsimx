

#' subset_toPlot
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
subset_toPlot <- function(DT = top5,
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
#' @param DT
#' @param treatment1
#' @param treatment2
#' @param title
#'
#' @import ggplot2
#' @import stringr
#'
#' @return
#' @export
#'
#' @examples
plot_params <- function(DT = top5, title, point_size = 2){
  palette = rep("grey", times = 2)
  palette_named = setNames(palette,  c("Predict SWC", "Observed SWC"))
  palette_named[2] = "red"
  if(is.null(dim(DT)) | nrow(DT) == 0){
    print(paste0(DT, " is empty!!!"))
  } else {

      DT %>%
        ggplot(aes(Date)) +
        geom_point(aes(y = PSWC, color = names(palette_named)[1]), size = point_size) +
        geom_point(aes(y = SWC,color = names(palette_named)[2]), size = point_size) +
        geom_text(aes(x = max(DT$Date) - 60, y = median(DT$PSWC),
                      label = paste0("NSE = ", NSE,
                                     "\r\nR.square = ", R2,
                                     "\r\nRMSE = ", RMSE)),inherit.aes = FALSE)+
        facet_grid(  KLR_RFV_SKL ~.) +
        ggtitle(paste0(unique(DT$Experiment), unique(DT$SowingDate))) +
        scale_x_date(date_labels = "%Y %b", date_breaks = "4 weeks") +
        scale_color_manual(name = "", values = palette_named) +
        theme_water() +
        theme(legend.position = "top",
              axis.text.x = element_text(angle = 30, hjust = 1)) +
        ggsave(str_glue("{title}.pdf"), width = 10, height = 10, dpi = 300)
    }

}
