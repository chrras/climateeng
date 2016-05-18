hum_ratio <- function(rel.hum, temp.air, alt = 0) {
  #' Humidity ratio (W)
  #'
  #' Calculates the humidity ratio from relative humidity and temperature.
  #' Eq. 25 & 12 - ASHRAE Fundamentals Handbook 2002, Psychrometrics.
  #' @param rel.hum Vector of relative humidities [\%].
  #' @param temp.air Vector of air temperatures [degC].
  #' @param alt Vector of altitudes [m]. Defaults to 0 m (sea level).
  #' @return Returns a vector of humidity ratios [kg/kg].
  #' @keywords indoor climate
  #' @export
  #' @examples
  #' hum_ratio(rel.hum = 60, temp.air = 25, alt = 0)
  #' @author Christoffer Rasmussen

  # Humidity ratio (eq. 25 and 12, solved for W)
  rel.hum <- rel.hum / 100
  w.s <- sat_hum_ratio(temp.air, alt)
  p <- bar_press(alt)
  p.ws <- sat_w_press(temp.air)

  return((-rel.hum) * w.s * (p - p.ws) / (rel.hum * p.ws - p))
}
