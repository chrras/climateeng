sat_hum_ratio <- function(temp.air, alt = 0) {
  #' Humidity ratio at satuation (W_s)
  #'
  #' Eq. 23 - ASHRAE Fundamentals Handbook, Psychrometrics.
  #' @param temp.air Vector of air temperatures [degC].
  #' @param alt Vector of altitudes [m]. Defaults to 0 m (sea level).
  #' @return Returns a vector of water vapour content at satuatuation [kg/kg].
  #' @keywords indoor climate
  #' @export
  #' @examples
  #' sat_hum_ratio(temp.air = 25, alt = 0)
  #' @author Christoffer Rasmussen, MSc in Engineering

  # Humidity ratio i kg/kg (eq. 23)
  return(0.62198 * sat_w_press(temp.air) / (bar_press(alt) - sat_w_press(temp.air)))
}
