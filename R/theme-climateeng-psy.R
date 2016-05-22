theme_climateeng_psy <- function(asp = NULL) {
  #' Custom ggplot2
  #'
  #' Custom theme for climateeng psychrometric chart.
  #' @param asp Aspect ratio of plot. Defaults to NULL.
  #' @return Returns a ggplot2 theme.
  #' @keywords internal
  #' @export
  #' @examples
  #' theme_climateeng_psy()
  #' @author Christoffer Rasmussen

  # Generate the colors for the chart procedurally with RColorBrewer
  theme <- theme_parms()

  # Begin construction of chart
  theme_bw(base_size = 9) +

    # Set the entire chart region to a light gray color
    theme(panel.background = element_rect(fill = theme$color.background,
                                          color = theme$color.background)) +
    theme(plot.background = element_rect(fill = theme$color.background,
                                         color = theme$color.background)) +
    theme(panel.border = element_rect(color = theme$color.background)) +

    # Format the grid
    theme(panel.grid = element_blank()) +
    theme(axis.ticks = element_line(NA)) +

    # Format the legend, but hide by default
    theme(legend.position = "none") +
    theme(legend.background = element_rect(fill = theme$color.background)) +
    theme(legend.text = element_text(size = 7,
                                     color = theme$color.axis.title)) +

    # Set title and axis labels, and format these and tick marks
    theme(plot.title = element_text(color = theme$color.title,
                                    size = 10,
                                    vjust = 1.25)) +
    theme(axis.text.x = element_text(size = 7,
                                     color = theme$color.axis.text)) +
    theme(axis.text.y = element_text(size = 7,
                                     color = theme$color.axis.text)) +
    theme(axis.title.x = element_text(size = 8,
                                      color = theme$color.axis.title,
                                      vjust = 0)) +
    theme(axis.title.y = element_text(size = 8,
                                      color = theme$color.axis.title,
                                      vjust = 1.25)) +

    theme(aspect.ratio = asp) +

    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))

}
