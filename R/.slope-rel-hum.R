slope_rel_hum <- function(temp.1, temp.2, rel.hum) {
  #' Slope of relative humidity line
  #'
  #' Calculate slope of relative humidity line.
  #' @param temp.1 Temperature of first point [degC].
  #' @param temp.2 Temperature of second point [degC].
  #' @param rel.hum Relative humidity line to find slope of [\%].
  #' @return Returns the slope angle [deg].
  #' @keywords internal
  #' @examples
  #' slope_rel_hum(15, 20, 80)
  #' @author Christoffer Rasmussen

  x1 <- temp.1
  x2 <- temp.2
  y1 <- hum_ratio(rel.hum, temp.1)
  y2 <- hum_ratio(rel.hum, temp.2)

  angle <- atan((y2 - y1) * 1000 / (x2 - x1))

  return(angle * 180 / pi)
}
