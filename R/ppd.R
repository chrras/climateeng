ppd <- function(clo, temp.air, temp.rad, rel.hum, met = 1.2, wme = 0,
                air.velo = 0.1, partial.press = 0) {
  #' Predicted percentage dissatisfied (ppd)
  #'
  #' Vectorized calculation of the predicted percentage dissatisfied (PPD)
  #'  according to ASHRAE 55-2010.
  #' @param temp.air Vector of air temperatures [degC].
  #' @param temp.rad Vector of mean radiant temperatures [degC].
  #' @param rel.hum Vector of relative humidities in percent [\%].
  #' @param met Vector of metabolic rates [met]. Defaluts to 1.2 met.
  #' @param wme Vector of external work [met]. Defaults to 0 met.
  #' @param air.velo Vector of relative air velocities [m/s]. Defaults to
  #'  0.1 m/s.
  #' @param partial.press Vector of water vapour pressures in [Pa]. Defaults to
  #'  0 Pa.
  #' @return Returns a vector of PPD values.
  #' @export
  #' @examples
  #' temp.rad <- seq(20, 30)
  #' ppd(clo = 0.5, temp.air = 25, temp.rad = temp.rad, rel.hum = 50)
  #'
  #' # Validation (ASHRAE 55-2010, Appendix D.)
  #'
  #' clo <- rep(c(1, 0.5), each = 4)
  #' temp.air <- c(19.6, 23.9, 25.7, 21.2, 23.6, 26.8, 27.9, 24.7)
  #' temp.rad <- temp.air
  #' rel.hum <- c(86, 66, 15, 20, 67, 56, 13, 16)
  #' met <- 1.1
  #' wme <- 0
  #' air.velo <- 0.1
  #' partial.press <- 0
  #'
  #' ppd <- ppd(clo, temp.air, temp.rad, rel.hum, met, wme, air.velo,
  #'  partial.press)
  #' round(ppd, 0)
  #'
  #' # Validation (DS/EN ISO 7730-2006, Annex D).
  #'
  #' clo <- c(rep(c(0.5, 1), each = 5), rep(0.5, 3))
  #' temp.air <- c(22, 27, 27, 23.5, 23.5, 19, 23.5, 23.5, 23, 23, 22, 27, 27)
  #' temp.rad <- c(22, 27, 27, 25.5, 25.5, 19, 23.5, 23.5, 21, 21, 22, 27, 27)
  #' rel.hum <- c(rep(c(60, 40), each = 5), rep(60, 3))
  #' met <- c(rep(1.2, 10), rep(1.6, 3))
  #' wme <- 0
  #' air.velo <- c(0.1, 0.1, 0.3, 0.1, 0.3, 0.1, 0.1, 0.3, 0.1, 0.3, 0.1, 0.1,
  #'               0.3)
  #' partial.press <- 0
  #'
  #' ppd <- ppd(clo, temp.air, temp.rad, rel.hum, met, wme, air.velo,
  #'  partial.press)
  #' round(ppd, 0)
  #' @author Christoffer Rasmussen

  PMV <- pmv(clo, temp.air, temp.rad, rel.hum, met, wme, air.velo,
             partial.press)
  PPD <- 100 - 95 * exp(-0.03353 * PMV ** 4 - 0.2179 * PMV ** 2)

  return(PPD)
}
