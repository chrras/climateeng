enthalpy <- function(temp.db, hum.ratio) {
  #' Enthalpy (h)
  #'
  #' Calculates the enthalpy of a given moist air state.
  #' Eq. 32 - ASHRAE Fundamentals Handbook 2002, Psychrometrics.
  #' @param temp.db Vector of dry bulb temperatures [degC].
  #' @param hum.ratio Vector of humidity ratios [kg/kg].
  #' @return Returns a vector of enthalpies [kJ/kg].
  #' @keywords indoor climate
  #' @export
  #' @examples
  #' enthalpy(25, 0.010)
  #'
  #' # Calculating cooling demand
  #' density <- 1.2 # density of air (kg/m3)
  #' (enthalpy(35, 0.020) - enthalpy(15, 0.010)) * density
  #' @author Christoffer Rasmussen

  # Enthalpy (eq. 32)
  return(1.006 * temp.db + hum.ratio * (2501 + 1.805 * temp.db))
}
