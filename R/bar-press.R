bar_press <- function(alt = 0) {
  #' Barometric/total pressure (p)
  #'
  #' Calculates barometric pressure based on altitude. This is also called total
  #'  pressure.
  #' Eq. 3 - ASHRAE Fundamentals Handbook 2002, Psychrometrics.
  #' @param alt Vector of altitudes [m]. Defaults to 0 m (sea level).
  #' @return Returns a vector of barometric pressure [kPa].
  #' @keywords indoor climate
  #' @export
  #' @examples
  #' bar_press()
  #'
  #' bar_press(alt = 1000)
  #' @author Christoffer Rasmussen

  # Barometric pressure (eq. 3)
  return(101.325 + (1 - 2.25577e-05 * alt) ** 5.2559)
}
