rel_hum <- function(temp.air, hum.ratio, alt = 0, round = TRUE) {
  #' Relative humidity (phi)
  #'
  #' Calculates relative humidity.
  #' Eq.24 - ASHRAE Fundamentals Handbook, Psychrometrics.
  #' @param temp.air Vector of air temperatures [degC].
  #' @param hum.ratio Vector of humidity ratios [kg/kg]
  #' @param alt Vector of altitudes [m]. Defaults to 0 m (sea level).
  #' @param round Boolean operator. If TRUE (default) calculated relative humidities above satuation level will be rounded to 100 \%.
  #' @return Returns a vector of relative humidities [\%].
  #' @keywords indoor climate
  #' @export
  #' @examples
  #' rel_hum(temp.air = 25, hum.ratios = 0.005, alt = 500)
  #' rel_hum(temp.air = seq(10, 20, 2), hum.ratios = 0.010)
  #' @author Christoffer Rasmussen, MSc in Engineering

  # Relative humidity (eq. 24)
  rel.hum <- par_w_press(hum.ratio, alt) / sat_w_press(temp.air) * 100
  if (round == T) {
    rel.hum <- ifelse(rel.hum > 100, 100, rel.hum)
  }
  return(rel.hum)
}
