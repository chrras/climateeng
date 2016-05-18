pmv <- function(clo, temp.air, temp.rad, rel.hum, met = 1.2, wme = 0, air.velo = 0.1, par.w.press = 0) {
  #' Predicted mean vote (pmv)
  #'
  #' Vectorized calculation of the predicted mean vote (PMV) according to ASHRAE 55-2010.
  #' @param temp.air Vector of air temperatures [degC].
  #' @param temp.rad Vector of mean radiant temperatures [degC].
  #' @param rel.hum Vector of relative humidities in percent [\%].
  #' @param met Vector of metabolic rates [met]. Defaluts to 1.2 met.
  #' @param wme Vector of external work [met]. Defaults to 0 met.
  #' @param air.velo Vector of relative air velocities [m/s]. Defaults to 0.1 m/s.
  #' @param par.w.press Vector of water vapour pressures in [Pa]. Defaults to 0 Pa.
  #' @return Returns a vector of PMV values.
  #' @keywords indoor climate
  #' @export
  #' @examples
  #' temp.rad <- seq(20, 30)
  #' pmv(clo = 0.5, temp.air = 25, temp.rad = temp.rad, rel.hum = 50)
  #'
  #' # Validation (ASHRAE 55-2010, Appendix D).
  #'
  #' clo <- rep(c(1, 0.5), each = 4)
  #' temp.air <- c(19.6, 23.9, 25.7, 21.2, 23.6, 26.8, 27.9, 24.7)
  #' temp.rad <- temp.air
  #' rel.hum <- c(86, 66, 15, 20, 67, 56, 13, 16)
  #' met <- 1.1
  #' wme <- 0
  #' air.velo <- 0.1
  #' par.w.press <- 0
  #'
  #' pmv <- pmv(clo, temp.air, temp.rad, rel.hum, met, wme, air.velo, par.w.press)
  #' round(pmv, 1)
  #'
  #' # Validation (DS/EN ISO 7730-2006, Annex D).
  #'
  #' clo <- c(rep(c(0.5, 1), each = 5), rep(0.5, 3))
  #' temp.air <- c(22, 27, 27, 23.5, 23.5, 19, 23.5, 23.5, 23, 23, 22, 27, 27)
  #' temp.rad <- c(22, 27, 27, 25.5, 25.5, 19, 23.5, 23.5, 21, 21, 22, 27, 27)
  #' rel.hum <- c(rep(c(60, 40), each = 5), rep(60, 3))
  #' met <- c(rep(1.2, 10), rep(1.6, 3))
  #' wme <- 0
  #' air.velo <- c(0.1, 0.1, 0.3, 0.1, 0.3, 0.1, 0.1, 0.3, 0.1, 0.3, 0.1, 0.1, 0.3)
  #' par.w.press <- 0
  #'
  #' pmv <- pmv(clo, temp.air, temp.rad, rel.hum, met, wme, air.velo, par.w.press)
  #' round(pmv, 2)
  #' @author Christoffer Rasmussen

  air.velo <- ifelse(air.velo < 0, 0, air.velo)

  sat.w.press = exp(16.6536 - 4030.183 / (temp.air + 235)) # Saturated vapour pressure [kPa]

  if (par.w.press == 0) {
    par.w.press = rel.hum * 10 * sat.w.press
  }

  ICL = 0.155 * clo  # Thermal insulation of the clothing [m2*K/W]
  M = met * 58.15    # Metabolic rate [W/m2]
  W = wme * 58.15    # External work [W/m2]
  MW = M - W         # Internal heat production in the human body [W/m2]

  # Clothing area factor [-]
  FCL = ifelse(ICL <= 0.078, 1 + 1.29 * ICL, 1.05 + 0.645 * ICL)

  HCF = 12.1 * sqrt(air.velo)  # Heat transf. coeff. by forced convection
  TAA = temp.air + 273          # Air temperatuer [K]
  TRA = temp.rad + 273          # Mean radiant temperature [K]

  # CALCULATE SURFACE TEMPERATURE OF CLOTHING BY ITERATION

  # First guess for surface temperature of clothing
  TCLA = TAA + (35.5 - temp.air) / (3.5 * (6.45 * ICL + 0.1))

  # Calculation terms
  P1 = ICL * FCL
  P2 = P1 * 3.96
  P3 = P1 * 100
  P4 = P1 * TAA
  P5 = 308.7 - 0.028 * MW + P2 * (TRA / 100)^4

  XN = TCLA / 100
  XF = XN

  n = 0          # Number of iterations
  EPS = 0.00015  # Stop criteria in iteration

  repeat{

    XF = (XF + XN) / 2

    # Heat transf. coeff. by natural convection
    HCN = 2.38 * abs(100 * XF - TAA)^0.25

    HC = ifelse(HCF > HCN, HCF, HCN)
    XN = (P5 + P4 * HC - P2 * XF^4)/(100 + P3 * HC)
    n = n + 1

    # Exit iteration if not converged after 150 runs
    if (n > 150) {
      PMV = NA
      PPD = NA
      break
    }

    # Exit iteration if converged
    if(abs(max(XN, na.rm=TRUE) - min(XF, na.rm=TRUE)) < EPS){
      break
    }
  }

  TCL = 100 * XN - 273  # Surface temperature of clothing [°C]

  # HEAT LOSS COMPONENTS

  HL1 = 3.05 * 0.001 * (5733 - 6.99 * MW - par.w.press)      # Heat loss through skin
  HL2 = ifelse(MW > 58.15, 0.42 * (MW - 58.15), 0)  # Heat loss by sweating
  HL3 = 1.7 * 0.00001 * M * (5867 - par.w.press)             # Latent respi. heat loss
  HL4 = 0.0014 * M * (34 - temp.air)                      # Dry respiration heat loss
  HL5 = 3.96 * FCL * (XN^4 - (TRA / 100)^4)         # Heat loss by radiation
  HL6 = FCL * HC * (TCL - temp.air)                       # Heat loss by convection

  #  CALCULATION OF PMV AND PPD

  TS = 0.303 * exp(-0.036 * M) + 0.028  # Thermal Sensation trans. coeff.
  PMV = TS * (MW - HL1 - HL2 - HL3 - HL4 - HL5 - HL6)
  # PPD = 100 - 95 * exp(-0.03353 * PMV^4 - 0.2179 * PMV^2)
  return(PMV)
}
