# Solar geometry and radiation

yday365 <- function(yday) {
  # leap year normalization
  yday[yday == 366L] <- 365L
  stopifnot(all(yday >= 1L))
  stopifnot(all(yday <= 365L))
  return(yday)
}


# solar_declination <- function(yday, check_yday = TRUE) {
#   # theta.d - solar declination in radians
#   if (check_yday) yday <- yday365(yday)
#   return(23.45 * pi / 180 * sinpi(2 * (284 + yday) / 365))
# }

eq_time <- function(yday, check_yday = TRUE) {

  if (check_yday) yday <- yday365(yday)
  y <- rep(NA, length(yday))
  ii <- yday <= 106
  y[ii] <- -14.2 * sinpi((yday[ii] + 7) / 111)
  ii <- yday > 106 & yday <= 166
  y[ii] <- 4.0 * sinpi((yday[ii] - 106) / 59)
  ii <- yday > 166 & yday <= 247
  y[ii] <- -6.5 * sinpi((yday[ii] - 166) / 80)
  ii <- yday > 247
  y[ii] <- 16.4 * sinpi((yday[ii] - 247) / 113)
  #
  return(y)
  # https://github.com/bnasr/solrad/blob/master/R/EOT.R
  # B  <-  (yday - 81) * 360 / 365
  # ET <- 9.87*sin(pi/180*2*B) - 7.53*cos(pi/180*B) - 1.5*sin(pi/180*B)
  # ET
}

solar_time <- function(hour, E.qt, lon) {
  # T.solar - the apparent solar time
  hour + 0.5 + E.qt / 60 + lon / 15
}

hour_angle <- function(T.solar) {
  # theta.hr - the hour angle in radians
  return(pi * (T.solar - 12) / 12)
}

zenith <- function(lat, theta.d = NULL, theta.hr = NULL,
                   yday = NULL, # required for theta.d and theta.hr
                   lon = NULL, hour = NULL # required for theta.hr
) {
  # solar zenith angle (in degrees, from 0 to 180 degree,
  # with 0 when sun at zenith direction and 90 when sun at the horizon)
  if (is.null(theta.d)) {
    if (is.null(yday)) stop("Either 'theta.d' or 'yday' should be provided")
    theta.d <- solar_declination(yday)
  }
  if (is.null(theta.hr)) {
    if (any(is.null(yday), is.null(lon), is.null(hour))) {
      stop("Either 'theta.hr' or all of 'yday', 'lon', 'hour' should be provided")
    }
    T.solar <- solar_time(hour = hour, E.qt = eq_time(yday), lon = lon)
    theta.hr <- hour_angle(T.solar)
  }
  zenith <- acos(sinpi(lat / 180) * sin(theta.d) +
                   cospi(lat / 180) * cos(theta.d) * cos(theta.hr)) / pi * 180
  return(zenith)
}

azimuth <- function(yday, lon, lat, hour, check_yday = TRUE) {
  # solar azimuth angle (in degrees, from 0 to 360 degree,
  # with 0 degree at north and 90 degree at east)
  # given in horizontal coordinate system
  # checks
  if (check_yday) yday <- yday365(yday)
  
  E.qt <- eq_time(yday, check_yday = FALSE)
  T.solar <- solar_time(hour, E.qt, lon)
  theta.d <- solar_declination(yday, check_yday = FALSE)
  theta.hr <- hour_angle(T.solar)
  zenith <- zenith(lat, theta.d, theta.hr)
  # A
  sin.azimuth <- round(-sin(theta.hr) * cos(theta.d) /
                         sinpi(zenith / 180), digits = 12)
  # B
  cos.azimuth <- round(
    (sin(theta.d) - sinpi(lat / 180) * cospi(zenith / 180)) /
      (cospi(lat / 180) * sinpi(zenith / 180)),
    digits = 12
  )
  
  azimuth <- 180 / pi * (
    (sin.azimuth >= 0) * (cos.azimuth >= 0) * asin(sin.azimuth) +
      (cos.azimuth < 0) * (pi - asin(sin.azimuth)) +
      (sin.azimuth < 0) * (cos.azimuth >= 0) * (2 * pi + asin(sin.azimuth))
  )
  # rm(sin.azimuth,cos.azimuth)
  return(azimuth)
}

# solar_position <- function(x) {
#   UseMethod("solar_position")
# }
# 
# solar_position.data.frame <- function(x) {
#   
# }

#' Solar position
#'
#' @param datetime date and time
#' @param x 
#' @param hour 
#' @param lon 
#' @param lat 
#' @param verbose 
#' @param yday day of a year
#'
#' @details  
#' \loadmathjax
#' List or data.frame with estimated following solar geometry variables:
#' \itemize{
#'   \item Solar declination (\mjseqn{\theta_d})
#'     \mjsdeqn{\theta_d = \frac{23.45\pi}{180}\sin{\big(2\pi\frac{284+n}{365}\big)}} 
#'     
#'   \item Equation of time (\mjseqn{E_{qt}})
#'      \mjsdeqn{
#'        E_{qt} = \begin{cases}
#'          -14.2\sin{\big(\frac{\pi(n+7)}{111}\big)}& & {1 \leq n \leq 106}\newline 
#'          4.0\sin{\big(\frac{\pi(n-106)}{59}\big)}& & {107 \leq n \leq 166}\newline 
#'          -6.5\sin{\big(\frac{\pi(n-166)}{80}\big)}& & {167 \leq n \leq 246}\newline 
#'          16.4\sin{\big(\frac{\pi(n-247)}{113}\big)}& & {247 \leq n \leq 365}
#'          \end{cases}
#'        }
#'        
#'   \item Apparent solar time (\mjseqn{T_{solar}})
#'      \mjsdeqn{T_{solar} = T_{UTC}+\frac{E_{qt}}{60}+longitude/15}
#'      
#'   \item Hour angle (\mjseqn{\theta_{hr}})
#'      \mjsdeqn{\theta_{hr} = \frac{T_{solar}-12}{12}\pi}
#'        
#'   \item Zenith angle
#'      \mjsdeqn{zenith = \arccos\big({\sin{(latitude)}\sin{(\theta_d)}+\cos{(latitude)}\cos{(\theta_d)}\cos{(\theta_{hr})}\big)}}
#'      
#'   \item Azimuth angle
#'      \mjsdeqn{
#'        azimuth = \begin{cases}
#'          \arcsin{(A)}& & {A \geq 0, B \geq 0}\newline
#'          180-\arcsin{(A)}& & {B < 0}\newline
#'          360+\arcsin{(A)}& & {A < 0, B \geq 0}
#'          \end{cases}
#'      }
#'      where \cr
#'             \mjseqn{n - \text{day of the year}} \cr
#'             \mjseqn{A = \sin{(azimuth)} = 
#'                        -\frac{\sin{(\theta_{hr})}\cos{(\theta_d)}}{\sin{(zenith)}}} \cr
#'             \mjseqn{B = \cos{(azimuth)} = \frac{\sin{(\theta_d)}-
#'                         \sin{(latitude)}\cos{(zenith)}}{\cos{(latitude)}\sin{(zenith)}}}
#' }
#' 
#' @return
#' 
#' @export
#' @import mathjaxr
#'
#' @examples
#' NA
solar_position <- function(x,
                           datetime = "datetime",
                           yday = "yday", hour = "hour", 
                           lon = "lon", lat = "lat",
                           keep.all = TRUE, verbose = getOption("merra2.verbose")) {
  # browser()
  stopifnot(!is.null(x[[lon]]))
  stopifnot(!is.null(x[[lat]]))
  # if (is.null(x[["datetime"]])) {
  #   stopifnot(is.null(x[[yday]]))
  #   stopifnot(is.null(x[[hour]]))
  # } else {
    # x <- data.table(
    #   datetime = datetime,
    #   yday = yday,
    #   hour = hour, 
    #   lon = lon,
    #   lat = lat
    # )
  # }
  # rm(datetime, yday, hour, lon, lat)
  # attach(x)
  # if (verbose) {
  #   cat("nrow(x) = ", nrow(x),"\n")
  #   cat("Calculating:\n")
  # }
  if (is.null(x[[datetime]])) {
    if (is.null(x[[yday]]) || is.null(x[[hour]])) {
      stop("Either 'datetime' or both 'yday' & 'hour' should be defined")
    }
  } else {
    if (!is.null(x[[yday]]) || !is.null(x[[hour]])) {
      # stop("Only one of 'datetime' or ('yday' & 'hour') should be defined")
      warning("Using 'datetime', ignoring 'yday' & 'hour'")
    }
    
    if (any(lubridate::tz(x[[datetime]]) != "UTC")) {
      if (verbose) message("   Changing the timezone of 'datetime' to UTC\n")
      x[["datetime"]] <- lubridate::with_tz(x[["datetime"]], tzone = "UTC")
    }
    if (verbose) cat("   The day of the year (yday) and the hour (hour) at UTC timezone\n")
    x[[yday]] <- lubridate::yday(x[[datetime]])
    x[[hour]] <- lubridate::hour(x[[datetime]])
    # rm(datetime)
  }
  stopifnot(!is.null(x[[lon]]))
  stopifnot(!is.null(x[[lat]]))
  
  if (any(x[[yday]] == 366)) {
    if (verbose) cat("   Leap year adjustment (yday 366 -> 365)\n")
    x[[yday]] <- yday365(x[[yday]])
  }

  # solar declination ####
  # x$declination <- solar_declination(yday = x[[yday]], check_yday = FALSE)
  if (verbose) cat("   Solar declination (declination)\n")
  declination <- 23.45 * pi / 180 * sinpi(2 * (284 + x[[yday]]) / 365)
  # Equation of time ####
  # x$eq_time <- eq_time(yday = x[[yday]], check_yday = FALSE)
  if (verbose) cat("   Equation of time (eq_time)\n")
  eq_time <- rep(NA, length(x[[yday]]))
  ii <- x[[yday]] <= 106
  eq_time[ii] <- -14.2 * sinpi((x[[yday]][ii] + 7) / 111)
  ii <- x[[yday]] > 106 & x[[yday]] <= 166
  eq_time[ii] <- 4.0 * sinpi((x[[yday]][ii] - 106) / 59)
  ii <- x[[yday]] > 166 &  x[[yday]] <= 247
  eq_time[ii] <- -6.5 * sinpi((x[[yday]][ii] - 166) / 80)
  ii <- x[[yday]] > 247
  eq_time[ii] <- 16.4 * sinpi((x[[yday]][ii] - 247) / 113)
  # Apparent solar time ####
  # x$solar_time <- solar_time(hour = x[[hour]], E.qt = x$eq_time, lon = x[[lon]])
  if (verbose) cat("   Apparent solar time (solar_time)\n")
  solar_time <- x[[hour]] + 0.5 + eq_time / 60 + x[[lon]] / 15
  # Hour angle ####
  # x$hour_angle <- hour_angle(x$solar_time)
  if (verbose) cat("   Hour angle (hour_angle)\n")
  hour_angle <- pi * (solar_time - 12) / 12
  # Zenith angle ####
  # x$zenith <- zenith(lat = x[[lat]], theta.d = x$declination, theta.hr = x$hour_angle)
  if (verbose) cat("   Zenith angle (zenith)\n")
  zenith <- acos(sinpi(x[[lat]] / 180) * sin(declination) +
                   cospi(x[[lat]] / 180) * cos(declination) * 
                   cos(hour_angle)) / pi * 180
  # Azimuth angle ####
  # x$azimuth <- azimuth(yday = x[[yday]], lon = x[[lon]], lat = x[[lat]], )
  if (verbose) cat("   Azimuth angle (azimuth)\n")
  # A
  sin.azimuth <- round(-sin(hour_angle) * cos(declination) /
                         sinpi(zenith / 180), digits = 12)
  sin.azimuth[is.nan(sin.azimuth)] <- 0 # temporary solution for poles
  # B
  cos.azimuth <- round(
    (sin(declination) - sinpi(x[[lat]] / 180) * cospi(zenith / 180)) /
      (cospi(x[[lat]] / 180) * sinpi(zenith / 180)),
    digits = 12)
  cos.azimuth[is.nan(cos.azimuth)] <- 0 # temporary solution for poles
  # browser()
  azimuth <- rep_len(NA, length(sin.azimuth))
  jj <- cos.azimuth >= 0 
  azimuth[!jj] <- 180 / pi * (pi - asin(sin.azimuth[!jj]))
  ii <- jj & sin.azimuth >= 0
  azimuth[ii] <- 180 / pi * asin(sin.azimuth[ii])
  ii <- jj & sin.azimuth < 0
  azimuth[ii] <-  180 / pi * (2 * pi + asin(sin.azimuth[ii]))
  rm(sin.azimuth, cos.azimuth, ii, jj)
  
  # if (verbose) cat("done\n")

  if (keep.all) {
    x$declination <- declination
    x$eq_time <- eq_time
    x$solar_time <- solar_time
    x$hour_angle <- hour_angle
  }
  x$zenith <- zenith
  x$azimuth <- azimuth
  
  return(x)
}

if (F) {
  # x <- left_join(merra2_mar, select(locid, 1:3))
  library(tidyverse)
  library(data.table)
  size <- energyRt::size
  
  x <- merra2_mar %>%
    # bind_rows(merra2_jan) %>%
    # bind_rows(merra2_feb) %>%
    # bind_rows(merra2_mar) %>%
    # bind_rows(merra2_apr) %>%
    # bind_rows(merra2_may) %>%
    # bind_rows(merra2_jun) %>%
    # bind_rows(merra2_jul) %>%
    # bind_rows(merra2_aug) %>%
    # bind_rows(merra2_sep) %>%
    # bind_rows(merra2_oct) %>%
    # bind_rows(merra2_nov) %>%
    # bind_rows(merra2_dec) %>%
    full_join(locid[,1:3])
  x
  size(x)
  
  y <- solar_position(x)
  summary(y$yday)
  summary(y$hour)
  summary(y$declination)
  summary(y$eq_time)
  summary(y$solar_time)
  summary(y$hour_angle)
  summary(y$zenith)
  # summary(y$zenith_avr)
  # summary(y$daytime)
  summary(y$azimuth)
  
  # mathjaxr::preview_rd("solar_position", type = "pdf")
  
  # y <- mutate(y, hour_angle_plus = pi * (12 - solar_time) / 12)
  # summary(y$hour_angle_plus)
  
  # y$zenith_plus <- acos(sinpi(y$lat / 180) * sin(y$declination) +
                   # cospi(y$lat / 180) * cos(y$declination) * cos(y$hour_angle_plus)) / pi * 180
  
  # ii <- sample(1:nrow(y), size = 1e4)
  
  # plot(y$zenith[ii], y$zenith_plus[ii])
  # summary(y$zenith[ii] - y$zenith_plus[ii])
  
}
