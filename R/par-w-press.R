par_w_press <- function(hum.ratio, alt = 0) {
  #' Partial water vapour pressure (p_w)
  #'
  #' Calculates partial water vapour pressure.
  #' Eq. 36 - ASHRAE Fundamentals Handbook 2002, Psychrometrics.
  #' @param hum.ratio Vector of humidity ratios [kg/kg].
  #' @param alt Vector of altitudes [m]. Defaults to 0 m (sea level).
  #' @return Returns a vector of partial water vapour pressure [kPa].
  #' @export
  #' @examples
  #' par_w_press(hum.ratio = 0.010)
  #'
  #' hum.ratio <- rep(c(0.005, 0.010), 2)
  #' alt <- rep(c(0, 500), each = 2)
  #' par_w_press(hum.ratio, alt)
  #' @author Christoffer Rasmussen

  # Partial water vapor pressure (eq. 36)
  return(bar_press(alt) * hum.ratio / (0.62198 + hum.ratio))
}
