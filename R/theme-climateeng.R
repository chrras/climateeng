theme_climateeng <- function() {
  #' Custom ggplot2 theme
  #'
  #' Custom theme for climateeng plots.
  #' @return Returns a ggplot2 theme.
  #' @keywords internal
  #' @export
  #' @examples
  #' theme_climateeng()
  #' @author Christoffer Rasmussen, MSc in Engineering

  # Generate the colors for the chart procedurally with RColorBrewer
  p <- theme_parms()

  # Begin construction of chart
  theme_bw(base_size=9) +

  # Set the entire chart region to a light gray color
  theme(panel.background=element_rect(fill=p$color.background, color=p$color.background)) +
  theme(plot.background=element_rect(fill=p$color.background, color=p$color.background)) +
  theme(panel.border=element_rect(color=p$color.background)) +

  # Format the grid
  theme(panel.grid.major.y=element_line(color=p$color.grid.major,size=p$size.line)) +
  theme(panel.grid.major.x=element_blank()) +
  theme(panel.grid.minor=element_blank()) +
  theme(axis.ticks=element_line(NA)) +

  # Format the legend, but hide by default
  theme(legend.position="none") +
  theme(legend.background = element_rect(fill=p$color.background)) +
  theme(legend.text = element_text(size=theme$text.axis,color=p$color.axis.title)) +

  # Set title and axis labels, and format these and tick marks
  theme(plot.title=element_text(color=p$color.title, size=theme$text.plot.title, vjust=1.25)) +
  theme(axis.text.x=element_text(size=theme$text.axis, color=p$color.axis.text)) +
  theme(axis.text.y=element_text(size=theme$text.axis, color=p$color.axis.text)) +
  theme(axis.title.x=element_text(size=theme$text.axis.title, color=p$color.axis.title, vjust=0)) +
  theme(axis.title.y=element_text(size=theme$text.axis.title, color=p$color.axis.title, vjust=1.25)) +

  # Plot margins
  theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))

}
