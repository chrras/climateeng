sat_w_press <- function(temp.air) {
  #' Satuated water vapour pressure (p_ws)
  #'
  #' Calculates the satuated water vapour pressure over water and ice.
  #' Eq.5 & 6 - ASHRAE Fundamentals Handbook 2002, Psychrometrics.
  #' @param temp.air Vector of air temperatures [degC].
  #' @return Returns a vector of satuated water pressure over water and ice [kPa].
  #' @keywords indoor climate
  #' @export
  #' @examples
  #' sat_w_press(temp.air = 25)
  #' @author Christoffer Rasmussen

  temp.air <- temp.air + 273.15

  # Coefficients for calculating the satuated pressure over water and ice
  c <- c(-5674.5359, 6.3925247, -0.009677843, 6.22157E-07, 2.07478E-09,
         -9.48402E-13, 4.1635019, -5800.2206, 1.3914993, -0.048640239,
         4.17648E-05, -1.44521E-08, 6.5459673)

  # Calculate satuated pressure over ice (Eq. 5) and water (Eq. 6)
  sat.w.press <- ifelse(temp.air < 273.15,
                 exp(c[1] / temp.air + c[2] + c[3] * temp.air + c[4] * temp.air**2 +
                       c[5] * temp.air**3 + c[6] * temp.air**4 + c[7] * log(temp.air)),
                 exp(c[8] / temp.air + c[9] + c[10] * temp.air + c[11] * temp.air**2 +
                       c[12] * temp.air**3 + c[13] * log(temp.air)))

  # Return pressure in kPa
  return(sat.w.press / 1000)
}
