hum_ratio_enthalpy <- function(enthalpy, temp.db, alt = 0) {
  #' Humidity ratio for enthalpy lines
  #'
  #' Calculates the humidity ratio based on the enthalpy.
  #' Eq. 32 - ASHRAE Fundamentals Handbook 2002, Psychrometrics.
  #' @param enthalpy Vector of enthalpies [kJ/kg].
  #' @param temp.db Vector of dry bulb temperatures [degC].
  #' @return Returns a vector of humidity ratios [kg/kg].
  #' @keywords internal
  #' @export
  #' @examples
  #' hum_ratio_enthalpy(enthalpy = 30, temp.db = 25)
  #' @author Christoffer Rasmussen

  # Humidity ratio (eq. 32, solved for W)
  w <- -0.557341 * (temp.db - 0.994036 * enthalpy) / (temp.db + 1385.6)

  cut.profile <- sat_hum_ratio(temp.db, alt)

  w <- ifelse(w > cut.profile, NA, ifelse(w < 0, NA, w))

  return(w)

}
