#' Create a R calendar
#'
#' Create a R calendar using geom_tile from ggplot.
#'
#' @param data data.frame that must contain different columns : \itemize{
#'   \item weekday : Day number of the current week.
#'   \item monthweek : Week number of the current month.
#'   \item valueCol : Value to be plotted in the calendar.
#'   \item dateCol : Date colum in format "dmy".
#' }
#'
#' @return \code{gg} object.
#' @export
#'
#' @examples
#'
#' @references all code come from https://vietle.info/post/calendarheatmap/
#'
#' @import ggplot2 dplyr magrittr scales
g_calendar <- function(data){

  g_df <- data %>%
    ggplot(aes(weekday, -monthweek, fill = valueCol)) +
    geom_tile(colour = "white") +
    geom_text(aes(label = day(dateCol)), size = 2.5, color = "black") +
    theme(aspect.ratio = 1/2,
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank(),
          strip.text = element_text(face = "bold", size = 12),
          panel.border = element_rect(colour = "grey", fill=NA, size=1),
          panel.spacing.x=unit(0, "lines")) +
    scale_fill_gradientn(colours = c("#6b9235", "white", "red"),
                         values = scales::rescale(c(-1, -0.05, 0, 0.05, 1)),
                         name = "Values",
                         guide = guide_colorbar(title.position = "top",
                                                direction = "horizontal")) +
    facet_wrap(~month, scales = "free", ncol = 6)

  return(g_df)
}



