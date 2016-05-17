hum_ratio_enthalpy <- function(enthalpy, temp.db, alt = 0) {
  #' Humidity ratio for enthalpy lines
  #'
  #' Calculates the humidity ratio based on the enthalpy.
  #' Eq. 32 - ASHRAE Fundamentals Handbook, Psychrometrics.
  #' @param enthalpy Vector of enthalpies [kJ/kg].
  #' @param temp.db Vector of dry bulb temperatures [degC].
  #' @return Returns a vector of humidity ratios [kg/kg].
  #' @keywords internal
  #' @export
  #' @examples
  #' hum_ratio_enthalpy(enthalpy = 30, temp.db = 25)
  #' @author Christoffer Rasmussen, MSc in Engineering

  # Humidity ratio (eq. 32)
  W <- -0.557341 * (temp.db - 0.994036 * enthalpy) / (temp.db + 1385.6)

  #cut.profile <- temp.db * slope + intersect
  cut.profile <- sat_hum_ratio(temp.db, alt)

  W <- ifelse(W > cut.profile, NA, ifelse(W < 0, NA, W))
  #W <- ifelse(W < 0, NA, W)

  return(W)

}
