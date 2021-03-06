% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/density-air.R
\name{density_air}
\alias{density_air}
\title{Density of moist air (rho)}
\usage{
density_air(temp.air, hum.ratio, alt = 0)
}
\arguments{
\item{temp.air}{Vector of air temperatures [degC].}

\item{hum.ratio}{Vector of humidity ratios [kg/kg]}

\item{alt}{Vector of altitudes [m]. Defaults to 0 m (sea level).}
}
\value{
Returns a vector of densities of moist air [kg/m3].
}
\description{
Calculates the density of moist air.
Eq. 11 - ASHRAE Fundamentals Handbook 2002, Psychrometrics.
}
\examples{
density_air(temp.air = 25, hum.ratio = c(0, 0.01, 0.02))
}
\author{
Christoffer Rasmussen
}

