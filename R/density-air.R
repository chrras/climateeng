density_air <- function(temp.air, hum.ratio, alt = 0) {
  #' Density of moist air (rho)
  #'
  #' Calculates the density of moist air.
  #' Eq. 11 - ASHRAE Fundamentals Handbook 2002, Psychrometrics.
  #' @param temp.air Vector of air temperatures [degC].
  #' @param hum.ratio Vector of humidity ratios [kg/kg]
  #' @param alt Vector of altitudes [m]. Defaults to 0 m (sea level).
  #' @return Returns a vector of densities of moist air [kg/m3].
  #' @export
  #' @examples
  #' density_air(temp.air = 25, hum.ratio = c(0, 0.01, 0.02))
  #' @author Christoffer Rasmussen

  # Density of moist air (eq. 11)
  v <- specific_vol(temp.air, hum.ratio, alt)
  return(1 / v * (1 + hum.ratio))
}
