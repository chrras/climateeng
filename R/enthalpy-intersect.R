enthalpy_intersect <- function(enthalpy, alt = 0) {
  #' Intersection between enthalpy line and 100 \% RH line
  #'
  #' Calculates the intersection of RH 100 \% line and the enthalpy line.
  #' @param enthalpy Vector of enthalpies [kJ/kg].
  #' @param alt Vector of altitudes [m]. Defaults to 0 m (sea level).
  #' @return Returns the dry-bulb temperature at which the enthalpy and
  #'  100 \% RH line intersects [degC].
  #' @keywords internal
  #' @export
  #' @examples
  #' enthalpy_intersect(enthalpy = 50)
  #' @author Christoffer Rasmussen

  temp <- seq(-75, 75, 0.1)

  df.test <- data.frame(
    temp = temp,
    enthalpy.diff =
      abs(-0.557341 * (temp - 0.994036 * enthalpy) / (temp + 1385.6) -
      sat_hum_ratio(temp, alt))
  )

  return(df.test$temp[which.min(df.test$enthalpy.diff)])
}
