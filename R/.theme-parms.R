theme_parms <- function() {
  #' ggplot2 parameters
  #'
  #' Parameters for climateeng themes.
  #' @return Returns a list of ggplot2 theme parameters.
  #' @keywords internal
  #' @examples
  #' theme_parms()
  #' @author Christoffer Rasmussen

  # Check for package -------------------------------------------------------

  if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
  stop("RColorBrewer is needed for this function to work. Please install it.",
       call. = FALSE)
  }

  # Set theme parameters ----------------------------------------------------

  palette <- RColorBrewer::brewer.pal("Greys", n = 9)

  l <- list(
    color.background = palette[1],
    color.grid.major = palette[4],
    color.axis.text = palette[6],
    color.axis.title = palette[7],
    color.title = palette[9],
    size.line = 0.25,
    text.size.axis = 7,
    text.size.axis.title = 8,
    text.size.plot.title = 10
  )

  return(l)
}
