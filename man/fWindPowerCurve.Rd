% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wind_power.R
\name{fWindPowerCurve}
\alias{fWindPowerCurve}
\alias{fWPC}
\title{Wind power curve function. Estimates power output based on the speed of wind.}
\usage{
fWindPowerCurve(mps, cutin = 3, cutoff = 25, data = NULL, ...)

fWPC(mps, cutin = 3, cutoff = 25, data = NULL, ...)
}
\arguments{
\item{mps}{wind speed, meters per second}

\item{cutin}{minimal speed of production}

\item{cutoff}{maximum speed of wind}

\item{data}{data frame or list with two power curve data, columns `speed` and `af`}
}
\value{
estimated capacity factor of a wind turbine
}
\description{
Wind power curve function. Estimates power output based on the speed of wind.
}
\examples{
fWPC(0:30)
plot(0:35, WindPowerCurve(0:35), type = "l", col = "red", lwd = 2)
}
