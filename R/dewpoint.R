dewpoint <- function(hum.ratio, alt = 0) {
  #' Dew-point temperature (t_d)
  #'
  #' Calculates the dew-point temperature based on humidity ratios.
  #' Eq. 37 & 38 - ASHRAE Fundamentals Handbook 2002, Psychrometrics.
  #' @param hum.ratio Vector of humidity ratios [kg/kg].
  #' @param alt Vector of altitudes [m]. Defaults to 0 m (sea level).
  #' @return Returns a vector of dew-point temperatures [degC].
  #' @keywords indoor climate
  #' @export
  #' @examples
  #' dewpoint(hum.ratio = 0.010)
  #'
  #' hum.ratio = seq(0, 0.050, by = 0.001)
  #' plot(dewpoint(hum.ratio = hum.ratio), hum.ratio,
  #'      xlab = "Dew-point [degC]",
  #'      ylab = "Humidity ratio [kg/kg]")
  #' @author Christoffer Rasmussen

  par.w.press <- par_w_press(hum.ratio, alt)

  # Dewpoint temperature (eq. 37)
  dewpoint <- 6.54 + 14.526*log(par.w.press) + 0.7389*log(par.w.press)**2 +
    0.09486*log(par.w.press)**3 + 0.4569*par.w.press**0.1984

  dewpoint <-
    ifelse(dewpoint < 0,
           6.09 + 12.608 * log(par.w.press) + 0.4959 * log(par.w.press)**2,
           ifelse(dewpoint > 93, NA, dewpoint))

  return(dewpoint)
}
