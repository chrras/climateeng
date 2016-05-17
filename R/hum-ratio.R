hum_ratio <- function(rel.hum, temp.air, alt = 0) {
  #' Humidity ratio (W)
  #'
  #' Calculates the humidity ratio from relative humidity and temperature.
  #' Eq. 25 & 12 - ASHRAE Fundamentals Handbook, Psychrometrics.
  #' @param rel.hum Vector of relative humidities [\%].
  #' @param temp.air Vector of air temperatures [degC].
  #' @param alt Vector of altitudes [m]. Defaults to 0 m (sea level).
  #' @return Returns a vector of humidity ratios [kg/kg].
  #' @keywords indoor climate
  #' @export
  #' @examples
  #' hum_ratio(rel.hum = 60, temp.air = 25, alt = 0)
  #' @author Christoffer Rasmussen, MSc in Engineering

  # Humidity ratio (eq. 25 and 12, solved for W)
  rel.hum <- rel.hum / 100
  W_s <- sat_hum_ratio(temp.air, alt)
  p <- bar_press(alt)
  p_ws <- sat_w_press(temp.air)

  return((-rel.hum) * W_s * (p - p_ws) / (rel.hum * p_ws - p))
}
