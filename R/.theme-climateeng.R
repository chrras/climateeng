theme_climateeng <- function() {
  #' Custom ggplot2 theme
  #'
  #' Custom theme for climateeng plots.
  #' @return Returns a ggplot2 theme.
  #' @keywords internal
  #' @examples
  #' theme_climateeng()
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
  p <- theme_parms()

  # Begin construction of chart
  ggplot2::theme_bw(base_size = 9) +

  # Set the entire chart region to a light gray color
    ggplot2::theme(panel.background = ggplot2::element_rect(
                     fill = p$color.background,
                     color = p$color.background)) +
    ggplot2::theme(plot.background = ggplot2::element_rect(
                     fill = p$color.background,
                     color = p$color.background)) +
    ggplot2::theme(panel.border = ggplot2::element_rect(
                     color = p$color.background)) +

  # Format the grid
    ggplot2::theme(panel.grid.major.y = ggplot2::element_line(
                     color = p$color.grid.major,
                     size = p$size.line)) +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank()) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank()) +
    ggplot2::theme(axis.ticks = ggplot2::element_line(NA)) +

  # Format the legend, but hide by default
    ggplot2::theme(legend.position = "none") +
    ggplot2::theme(legend.background = ggplot2::element_rect(
                     fill = p$color.background)) +
    ggplot2::theme(legend.text = ggplot2::element_text(
                     size = theme$text.axis,
                     color = p$color.axis.title)) +

  # Set title and axis labels, and format these and tick marks
    ggplot2::theme(plot.title = ggplot2::element_text(
                     color = p$color.title,
                     size = theme$text.plot.title,
                     vjust = 1.25)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(
                     size = theme$text.axis,
                     color = p$color.axis.text)) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(
                     size = theme$text.axis,
                     color = p$color.axis.text)) +
    ggplot2::theme(axis.title.x = ggplot2::element_text(
                     size = theme$text.axis.title,
                     color = p$color.axis.title,
                     vjust = 0)) +
    ggplot2::theme(axis.title.y = ggplot2::element_text(
                     size = theme$text.axis.title,
                     color = p$color.axis.title,
                     vjust = 1.25)) +

  # Plot margins
    ggplot2::theme(plot.margin = grid::unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}
