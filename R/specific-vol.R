specific_vol <- function(temp.db, hum.ratio, alt = 0) {
  #' Specific volume (v)
  #'
  #' Calculates the specific volume
  #' Eq. 28 (alternative) - ASHRAE Fundamentals Handbook 2002, Psychrometrics.
  #' @param temp.db Vector of dry bulb temperatures [degC].
  #' @param hum.ratio Vector of humidity ratios [kg/kg].
  #' @return Returns a vector of specific volumes [m3/kg].
  #' @keywords indoor climate
  #' @export
  #' @examples
  #' specific_vol(25, 0.010)
  #' @author Christoffer Rasmussen

  # Specific volume (eq. 28, alternative)
  return(0.2871 * (temp.db + 273.15) * (1 + 1.6078 * hum.ratio) / bar_press(alt))
}
