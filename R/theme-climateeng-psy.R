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

  # Check for package -------------------------------------------------------

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is needed for this function to work. Please install it.",
         call. = FALSE)
  } else if (!requireNamespace("grid", quietly = TRUE)) {
    stop("grid is needed for this function to work. Please install it.",
         call. = FALSE)
  }

  # Define theme -----------------------------------------------------------

  # Generate the colors for the chart procedurally with RColorBrewer
  theme <- theme_parms()

  # Begin construction of chart
  ggplot2::theme_bw(base_size = 9) +

    # Set the entire chart region to a light gray color
    ggplot2::theme(panel.background = ggplot2::element_rect(
                     fill = theme$color.background,
                     color = theme$color.background)) +
    ggplot2::theme(plot.background = ggplot2::element_rect(
                     fill = theme$color.background,
                     color = theme$color.background)) +
    ggplot2::theme(panel.border = ggplot2::element_rect(
                     color = theme$color.background)) +

    # Format the grid
    ggplot2::theme(panel.grid = ggplot2::element_blank()) +
    ggplot2::theme(axis.ticks = ggplot2::element_line(NA)) +

    # Format the legend, but hide by default
    ggplot2::theme(legend.position = "none") +
    ggplot2::theme(legend.background = ggplot2::element_rect(
                     fill = theme$color.background)) +
    ggplot2::theme(legend.text = ggplot2::element_text(
                     size = 7,
                     color = theme$color.axis.title)) +

    # Set title and axis labels, and format these and tick marks
    ggplot2::theme(plot.title = ggplot2::element_text(
                     color = theme$color.title,
                     size = 10,
                     vjust = 1.25)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(
                     size = 7,
                     color = theme$color.axis.text)) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(
                     size = 7,
                     color = theme$color.axis.text)) +
    ggplot2::theme(axis.title.x = ggplot2::element_text(
                     size = 8,
                     color = theme$color.axis.title,
                     vjust = 0)) +
    ggplot2::theme(axis.title.y = ggplot2::element_text(
                     size = 8,
                     color = theme$color.axis.title,
                     vjust = 1.25)) +

    ggplot2::theme(aspect.ratio = asp) +

    # Plot margins
    ggplot2::theme(plot.margin = grid::unit(c(0.35, 0.2, 0.3, 0.35), "cm"))

}
