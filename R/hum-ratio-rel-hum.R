hum_ratio_rel_hum <- function(rel.hum, temp.air, hum.ratio.max, alt = 0) {
  #' Humidity ratio (W)
  #'
  #' Calculates the humidity ratio from relative humidity and temperature.
  #' Eq. 25 & 12 - ASHRAE Fundamentals Handbook 2002, Psychrometrics.
  #' @param rel.hum Vector of relative humidities [\%].
  #' @param temp.air Vector of air temperatures [degC].
  #' @param hum.ratio.max Max humidity ratio on psychrometric chart [kg/kg].
  #' @param alt Vector of altitudes [m]. Defaults to 0 m (sea level).
  #' @return Returns a vector of humidity ratios [kg/kg].
  #' @keywords internal
  #' @export
  #' @examples
  #' hum_ratio_rel_hum(rel.hum = 60, temp.air = 25, hum.ratio.max = 0.02)
  #' @author Christoffer Rasmussen

  # Humidity ratio (eq. 25 and 12, solved for W)
  rel.hum <- rel.hum / 100
  W_s <- sat_hum_ratio(temp.air, alt)
  p <- bar_press(alt)
  p_ws <- sat_w_press(temp.air)

  W <- (-rel.hum) * W_s * (p - p_ws) / (rel.hum * p_ws - p)

  W <- ifelse(W > hum.ratio.max, NA, W)

  return(W)

}
