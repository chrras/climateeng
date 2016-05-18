wetbulb_intersect <- function(temp.wb, alt = 0) {
  #' Wet-bulb intersection with dry-bulb temperature axis
  #'
  #' Calculates the drybulb temperature at which the wetbulb temperature line
  #' intersects in the psychrometric chart.
  #' Eq. 32 - ASHRAE Fundamentals Handbook 2002, Psychrometrics.
  #' @param temp.wb Vector of wet-bulb temperatures [degC].
  #' @param alt Vector of altitudes [m]. Defaults to 0 m (sea level).
  #' @return Returns a vector of dry-bulb temperatures at which the wet-bulb
  #'   temperature line intersects [degC].
  #' @keywords internal
  #' @export
  #' @examples
  #' wetbulb_intersect(15)
  #' wetbulb_intersect(15, alt = 1000)
  #' @author Christoffer Rasmussen

  # Calculate satuated humidity for wet-bulb temperature (kg/kg)
  w.s <- sat_hum_ratio(temp.wb, alt)

  # Return dry-bulb temperature for given wet-bulb temperature (Eq. 35)
  return(-2.36581 * ((temp.wb - 1050.84) * w.s - 0.422689 * temp.wb))
}
