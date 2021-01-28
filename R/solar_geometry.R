#' Solar position
#'
#' @param x data.frame with MERRA-2 subset
#' @param UTC name (string) of the column in x with date and time in UTC format
#' @param yday optional name of column in x with the day of the year (consistent with UTC time), will be used if "UTC" is not provided
#' @param hour optional, the name of column with hour of the day (0 to 23, UTC time assumed)
#' @param lon longitude of the location
#' @param lat latitude of the location
#' @param verbose 
#' @param integral_steps integer number of steps for calculation of solar_time, hour_angle, and zenith within an hour, and  logical variable; default is 2 (start and the end of every hour)
#' @param keep.all if TRUE, the interim variables declination, eq_time, solar_time, and hour_angle will be added to x and returned
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
                           UTC = "UTC",
                           yday = "yday", hour = "hour",
                           lon = "lon", lat = "lat",
                           integral_steps = 2,
                           keep.all = TRUE, 
                           verbose = getOption("merra2.verbose"), ...) {
  # browser()
  # integral_steps = 3
  # Checks
  stopifnot(!is.null(x[[lon]]))
  stopifnot(!is.null(x[[lat]]))
  if (is.null(x[[UTC]])) {
    if (is.null(x[[yday]]) || is.null(x[[hour]])) {
      stop("Either 'UTC' or both 'yday' & 'hour' should be defined")
    }
  } else {
    if (!is.null(x[[yday]]) || !is.null(x[[hour]])) {
      # stop("Only one of 'UTC' or ('yday' & 'hour') should be defined")
      warning("Using 'UTC', ignoring 'yday' & 'hour'")
    }
    if (any(lubridate::tz(x[[UTC]]) != "UTC")) {
      if (verbose) message("   Changing the timezone of 'UTC' to UTC\n")
      x[["UTC"]] <- lubridate::with_tz(x[["UTC"]], tzone = "UTC")
    }
    if (verbose) cat("   The day of the year (yday) and the hour (hour) at UTC timezone\n")
    x[[yday]] <- lubridate::yday(x[[UTC]])
    x[[hour]] <- lubridate::hour(x[[UTC]])
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
  
  # Integral over hour
  if (integral_steps > 1) { # dh - weights (increment of every step)
    dh <- rep(1/(integral_steps - 1), integral_steps)
    h_i <- cumsum(c(0, dh[-integral_steps])) # hour steps
    dh[1] <- dh[1]/2; dh[integral_steps] <- dh[integral_steps]/2
  } else {
    dh <- 1 # weight
    h_i <- 0.5 # mid of hour
  }
  # browser()
  solar_time <- matrix(as.numeric(NA), nrow = nrow(x), ncol = integral_steps)
  hour_angle <- solar_time
  zenith <- solar_time
  sin.azimuth <- matrix(as.numeric(NA), nrow = nrow(x), ncol = 1)
  cos.azimuth <- sin.azimuth
  azimuth <- sin.azimuth
  # Apparent solar time ####
  if (verbose) cat("   Apparent solar time (solar_time) ")
  for (i in 1:integral_steps) {
    if (verbose) cat(i, " ", sep = "")
    solar_time[,i] <- x[[hour]] + h_i[i] + eq_time / 60 + x[[lon]] / 15 # + 0.5
  }
  if (verbose) cat("\n")
  # Hour angle ####
  if (verbose) cat("   Hour angle (hour_angle) ")
  for (i in 1:integral_steps) {
    if (verbose) cat(i, " ", sep = "")
    hour_angle[,i] <- pi * (solar_time[,i] - 12) / 12
  }
  if (verbose) cat("\n")
  # Zenith angle ####
  if (verbose) cat("   Zenith angle (zenith) ")
  for (i in 1:integral_steps) {
    if (verbose) cat(i, " ", sep = "")
    zenith[,i] <- sinpi(x[[lat]] / 180) * sin(declination) +
      cosd(x[[lat]]) * cos(declination) * 
      cos(hour_angle[,i])
    zenith[zenith[,i] > 1, i] <- 1
    zenith[zenith[,i] < -1, i] <- -1
    zenith[,i] <- acos(zenith[,i]) / pi * 180
  }
  if (verbose) cat("\n")
  # browser()
  if (verbose) cat("   Direct beam hour (beam)\n")
  x$beam <- rowSums(zenith < 90) == integral_steps
  # Integrating...
  if (verbose) cat("   Integrating...")
  # dh <- c(0L, 1L, 0L)
  zenith <- zenith %*% dh
  # zenith <- matrix(apply(zenith, 1, min, na.rm = T), ncol = 1)
  solar_time <- solar_time %*% dh
  hour_angle <- hour_angle %*% dh
  # solar_time <- matrix(x[[hour]] + 0.5 + eq_time / 60 + x[[lon]] / 15, ncol = 1) 
  # hour_angle <- matrix(pi * (solar_time - 12) / 12, ncol = 1)
  if (verbose) cat("ok\n")  
  # Azimuth angle ####
  # x$azimuth <- azimuth(yday = x[[yday]], lon = x[[lon]], lat = x[[lat]], )
  if (verbose) cat("   Azimuth angle (azimuth) ")
  # browser()
  # for (i in 1:integral_steps) {
  for (i in 1) {
    # cat(i, " ", sep = "")
    # A
    sin.azimuth[,i] <- -sin(hour_angle[,i]) * cos(declination) /
                           sinpi(zenith[,i] / 180)
    sin.azimuth[sin.azimuth[,i] < -1] <- -1
    sin.azimuth[sin.azimuth[,i] > 1] <- 1
    sin.azimuth[is.nan(sin.azimuth[,i]),i] <- 0 # temporary solution for poles
    # B
    cos.azimuth[,i] <-
      (sin(declination) - sind(x[[lat]]) * cosd(zenith[,i])) /
        (cosd(x[[lat]]) * sind(zenith[,i]))
    cos.azimuth[cos.azimuth[,i] < -1] <- -1
    cos.azimuth[cos.azimuth[,i] > 1] <- 1
    cos.azimuth[is.nan(cos.azimuth[,i]),i] <- 0 # temporary solution for poles
    # browser()
    # azimuth[,i] <- rep_len(NA, length(sin.azimuth[,i]))
    jj <- cos.azimuth[,i] >= 0 
    azimuth[!jj,i] <- 180 / pi * (pi - asin(sin.azimuth[!jj,i]))
    ii <- jj & sin.azimuth[,i] >= 0
    azimuth[ii,i] <- 180 / pi * asin(sin.azimuth[ii,i])
    ii <- jj & sin.azimuth[,i] < 0
    azimuth[ii,i] <-  180 / pi * (2 * pi + asin(sin.azimuth[ii,i]))    
  }
  if (verbose) cat("\n")
  rm(sin.azimuth, cos.azimuth, ii, jj)
  # Full hour beam logical variable

  # azimuth <- azimuth %*% dh
  
  # if (verbose) cat("done\n")
  # if (verbose) cat("   Integrating...")
  if (keep.all) {
    x$declination <- declination
    x$eq_time <- eq_time
    x$solar_time <- solar_time #%*% dh
    x$hour_angle <- hour_angle #%*% dh
  }
  x$zenith <- zenith #%*% dh
  x$azimuth <- azimuth #%*% dh
  # browser()
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
  # summary(y$beam)
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

# Internal functions #### 

yday365 <- function(yday) {
  # leap year normalization
  yday[yday == 366L] <- 365L
  stopifnot(all(yday >= 1L))
  stopifnot(all(yday <= 365L))
  return(yday)
}

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
  zenith <- acos(sind(lat) * sin(theta.d) +
                   cosd(lat) * cos(theta.d) * cos(theta.hr)) / pi * 180
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
    (sin(theta.d) - sind(lat) * cosd(zenith)) /
      (cosd(lat) * sind(zenith)),
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
