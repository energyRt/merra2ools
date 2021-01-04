.pv_array_type <- function(array.type) {
  .tracking_types <- c("fh", "fl", "th", "tl", "tv", "td")
  # "fh" # "Horizontal (h) fixed (f) arrays"
  # "fl" # "Tilted (l) fixed (f) arrays"
  # "th" # "Horizontal (h) single axis tracking (t) arrays"
  # "tl" # "Tilted (l) single axis tracking (t) arrays"
  # "tv" # "Vertical (v) single axis tracking (t) arrays"
  # "td" # "Dual (d) axis tracking (t) arrays"
  
   if (length(array.type) == 1) {
    if (grepl("ALL", array.type, ignore.case = T)) {array.type <- .tracking_types}
  } else if (is.null(array.type)) {
    array.type <- .tracking_types
  }
  return(array.type)
}

#' Photovoltaic Solar Panel Orientation and Performance Models
#'
#' @param lat latitude of PV location (\mjseqn{-90 \leq lat \leq 90})
#' @param azimuth solar azimuth angle for the PV location (\mjseqn{0 \leq azimuth < 360})
#' @param zenith solar zenith angle for the PV location (\mjseqn{0 \leq azimuth \leq 90})
#' @param x 
#' @param array.type 
#' @param suffix 
#' @param array.tilt.range.fl 
#' @param array.tilt.range.th 
#' @param array.tilt.range.tl 
#' @param array.tilt.range.tv 
#' @param array.tilt.range.td 
#' @param array.tilt.range.fh 
#'
#' @details 
#' \loadmathjax
#' \describe{
#'   \item{Fixed PV Panel (\mjseqn{*.fl})}
#'        {South-facing fixed solar PV with the tilted angle equal to the site's latitude.}
#'   
#'     \itemize{
#'       \item PV Tilted Angle (\mjseqn{array.tilt}, in degrees)
#'         \mjsdeqn{array.tilt = latitude}
#'       \item PV Azimuth Angle (\mjseqn{array.azimuth}, in degrees)
#'         \mjsdeqn{array.azimuth = 180}
#'     }
#'   
#'   \item{Horizontal Single-Axis PV Tracker (\mjseqn{*.th})}
#'        {A horizontal single-axis PV tracker with its axis in align 
#'        with the meridian direction and parallel to the ground.}
#'     \itemize{
#'       \item PV tilted angle under the optimal rotation strategy (\mjseqn{array.tilt}, in degrees)
#'         \mjsdeqn{array.tilt = \arctan{\big(\tan{(zenith)}\cos{(azimuth-array.azimuth)}\big)}}
#'       \item PV Azimuth Angle (\mjseqn{array.azimuth}, in degrees)
#'         \mjsdeqn{array.azimuth = 90 \textrm{ if } azimuth < 180}
#'         \mjsdeqn{array.azimuth = 270 \textrm{ if } azimuth \geq180}
#'     }
#'   
#'   \item{Vertical Single-Axis Tracker (\mjseqn{*.tv})}
#'        {A vertical single-axis PV tracker with its axis normal to the ground.}
#'     \itemize{
#'       \item PV tilted angle under the optimal rotation strategy (\mjseqn{array.tilt}, in degrees)
#'         \mjsdeqn{array.tilt = latitude}
#'       \item PV Azimuth Angle (\mjseqn{array.azimuth}, in degrees)
#'         \mjsdeqn{array.azimuth = azimuth}
#'     }
#'     
#'   \item{Tilted Single-Axis Tracker (\mjseqn{*.tl})}
#'        {A single-axis PV tracker with its axis parallel to the meridian direction and the axis tilted angle equal to the site’s latitude.}
#'     \itemize{
#'       \item PV tilted angle under the optimal rotation strategy (\mjseqn{array.tilt}, in degrees)
#'         \mjsdeqn{array.tilt = \arctan{\big(\frac{\tan{(zenith)}}
#'                 {\cos{(array.azimuth-180)}}\big)}+\delta\pi}
#'         where \mjseqn{\delta=0 \textrm{ when }  90 \leq array.azimuth \leq 270, 
#'               \textrm{ otherwise }\delta=1}
#'       \item PV Azimuth Angle (\mjseqn{array.azimuth}, in degrees)
#'         \mjsdeqn{array.azimuth = 180(1+\sigma)+\Delta tilt}
#'         where
#'        \mjsdeqn{\sigma = \begin{cases}
#'            1& & {\Delta tilt < 0, azimuth \geq 180}\newline
#'            0& & {\Delta tilt \times (azimuth-180) \geq 0}\newline
#'            -1& & {\Delta tilt > 0, azimuth < 180}
#'            \end{cases}}
#'         \mjsdeqn{\Delta tilt = \arctan{\frac{\sin{(zenith)}
#'                  \sin{(azimuth-180)}}{\cos{(\beta)}\sin{(latitude)}}}}
#'         \mjsdeqn{\cos{(\beta)} = \cos{(zenith)}\cos{(latitude)}+
#'                  \sin{(zenith)}\sin{(latitude)}\cos{(azimuth-180)}}
#'         
#'            }
#'         
#'   \item{Dual-Axis Tracker (\mjseqn{*.td})}
#'        {A dual-axis PV tracker.}
#'   
#'     \itemize{
#'       \item PV Tilted Angle under the Optimal Rotation Strategy (\mjseqn{array.tilt}, in degrees)
#'         \mjsdeqn{array.tilt = zenith}
#'       \item PV Azimuth Angle under the Optimal Rotation Strategy (\mjseqn{array.azimuth}, in degrees)
#'         \mjsdeqn{array.azimuth = azimuth}
#'     }
#'     
#'  }
#'     
#' @return
#' 
#' @export
#' @import mathjaxr
#'
#' @examples
#' NA
pv_array_position <- function(x, 
                              array.type = "fl", 
                              suffix = TRUE,
                              # lon = "lon", 
                              lat = "lat", 
                              azimuth = "azimuth", zenith = "zenith",
                              verbose = TRUE,
                              # array.azimuth = NULL,
                              array.tilt.range.fh = c(0., 0.),
                              array.tilt.range.fl = c(0., 60),
                              array.tilt.range.th = c(0., 45),
                              array.tilt.range.tl = array.tilt.range.th,
                              array.tilt.range.tv = array.tilt.range.fl,
                              array.tilt.range.td = array.tilt.range.fl) {
  # browser()
  # c("fh", "fl", "th", "tl", "tv", "td")
  array.type <- .pv_array_type(array.type)
  if (verbose) cat("   PV-array position, array.type: ")
  ii <- x[[zenith]] <= 90 & is.finite(x[[zenith]]) # over horizon
  for (i in array.type) {
    if (verbose) cat(i, " ", sep = "")
    y <- data.table(
      array.tilt = as.numeric(rep(NA, nrow(x))),
      array.azimuth = as.numeric(rep(NA, nrow(x)))
    )
    if (i == "fh") { 
      # fixed horizontal ####
      y$array.azimuth <- 0 # Southern hemisphere facing North
      y$array.azimuth[x[[lat]] > 0] <- 180 # Northern hemisphere facing South
      y$array.tilt <- 0
      y$array.tilt[y$array.tilt < array.tilt.range.fh[1]] <- array.tilt.range.fh[1]
      y$array.tilt[y$array.tilt > array.tilt.range.fh[2]] <- array.tilt.range.fh[2]
    } else if (i == "fl") { 
      # fixed tilted ####
      y$array.azimuth <- 0 # Southern hemisphere facing North
      y$array.azimuth[x[[lat]] > 0] <- 180 # Northern hemisphere facing South
      y$array.tilt <- abs(x[[lat]])
      y$array.tilt[y$array.tilt < array.tilt.range.fl[1]] <- array.tilt.range.fl[1]
      y$array.tilt[y$array.tilt > array.tilt.range.fl[2]] <- array.tilt.range.fl[2]
    } else if (i == "th") { 
      # horizontal tracking ####
      y$array.azimuth <- 90 + as.numeric(x[[azimuth]] > 180) * 180
      # ii <- x[[zenith]] < 90
      y$array.tilt[ii] <- 
        atan(abs(
          tan(x[[zenith]][ii] / 180 * pi) * 
            cospi((x[[azimuth]][ii] - y$array.azimuth[ii]) / 180)
          )) / pi * 180 
      y$array.tilt[!ii] <- 90
      y$array.tilt[y$array.tilt < array.tilt.range.th[1]] <- array.tilt.range.th[1]
      y$array.tilt[y$array.tilt > array.tilt.range.th[2]] <- array.tilt.range.th[2]
      
    } else if (i == "tl") { 
      # horizontal tilted tracking ####
      # browser()
      y$array.tilt <- abs(x[[lat]])
      # y$array.azimuth <- 180
      y$array.azimuth <- 0 # Southern hemisphere facing North
      y$array.azimuth[x[[lat]] > 0] <- 180 # Northern hemisphere facing South
      if (is.null(x[["AOI.fl"]])) {
        y[[zenith]] <- as.numeric(NA); y[[azimuth]] <- as.numeric(NA)
        y[[zenith]][ii] <- x[[zenith]][ii]
        y[[azimuth]][ii] <- x[[azimuth]][ii]
        y <- angle_of_incidence(y, array.type = "fl", suffix = TRUE,
                                azimuth = azimuth, zenith = zenith, verbose = FALSE)
        y[[zenith]] <- NULL; y[[azimuth]] <- NULL
      } else {
        y[["AOI.fl"]] <- x[["AOI.fl"]]
      }
      # y$AOI.fl <- (x$zenith < 90) *
      #   acos(round(cospi(x$zenith / 180) * cospi(y$array.tilt / 180) +
      #                sinpi(x$zenith / 180) * sinpi(y$array.tilt / 180) *
      #                cospi((x$azimuth - y$array.azimuth) / 180), digits = 12))
      
      y$delta.gamma <- as.numeric(NA)
      y$delta.gamma[ii] <- atan(sinpi(x[[zenith]][ii] / 180) *
                            sinpi((x[[azimuth]][ii] - 180) / 180) / 
                              cos(y$AOI.fl[ii]) / sinpi(x[[lat]][ii] / 180)) / pi * 180
      y$AOI.fl <- NULL
      # ii <- zenith <= 90
      # aaz <- y$array.azimuth[ii]
      y$array.azimuth <- NA
      y$array.azimuth[ii] <- 180 + y$delta.gamma[ii] +
        ((y$delta.gamma[ii] * (x[[azimuth]][ii] - 180)) < 0) * 
        (2 * ((x[[azimuth]][ii] - 180) >= 0) - 1) * 180
      y$delta.gamma <- NULL
      y$array.tilt <- array.tilt.range.tl[2]
      y$array.tilt[ii] <- (atan(tan(x[[zenith]][ii] / 180 * pi) / 
                                  cospi((y$array.azimuth[ii] - 180) / 180)) +
                       (cospi(y$array.azimuth[ii] / 180 - 1) < 0) * pi) / pi * 180
      y$array.tilt[y$array.tilt < array.tilt.range.tl[1]] <- array.tilt.range.tl[1]
      y$array.tilt[y$array.tilt > array.tilt.range.tl[2]] <- array.tilt.range.tl[2]

    } else if (i == "tv") { 
      # vertical (azimuth) tracking ####
      y$array.tilt <- abs(x[[lat]])
      y$array.azimuth <- x[[azimuth]]
      y$array.tilt[y$array.tilt < array.tilt.range.tv[1]] <- array.tilt.range.tv[1]
      y$array.tilt[y$array.tilt > array.tilt.range.tv[2]] <- array.tilt.range.tv[2]
      
    } else if (i == "td") { 
      # dual axes tracking ####
      y$array.tilt <- array.tilt.range.td[2]
      y$array.tilt[ii] <- x[[zenith]][ii]
      y$array.tilt[y$array.tilt < array.tilt.range.td[1]] <- array.tilt.range.td[1]
      y$array.tilt[y$array.tilt > array.tilt.range.td[2]] <- array.tilt.range.td[2]
      y$array.azimuth <- x[[azimuth]]
      
    } else {
      stop("Unknown array.type '", i, "'.")
    }
    # y -> x
    array.tilt <- "array.tilt"; array.azimuth <- "array.azimuth"
    if (suffix) {
      array.tilt <- paste0(array.tilt, ".", i)
      array.azimuth <- paste0(array.azimuth, ".", i)
      names(y) <- c(array.tilt, array.azimuth)
    }
    x[[array.tilt]] <- y$array.tilt
    x[[array.azimuth]] <- y$array.azimuth
    
  }
  if (verbose) cat("\n")
  return(x)
}


#' Angle of Incidence (AOI)
#'
#' @param azimuth the solar zenith angle, degrees
#' @param zenith the solar azimuth angle, degrees
#' @param array.tilt the PV tilt angle, degrees
#' @param array.azimuth the PV azimuth angle, degrees
#'
#' @details 
#' \loadmathjax
#'     \mjsdeqn{AOI = \arccos\big({\cos{(zenith)}\cos{(array.tilt)}+
#'              \sin{(zenith)}\sin{(array.tilt)}\cos{(azimuth-array.azimuth)}\big)}}
#'     Though the equation returns AOI values with any given set of zenith, azimuth, 
#'     array.tilt, and array.azimuth, the AOI is meaningful only if 
#'     \mjseqn{0 \leq zenith \leq 90}, and \mjseqn{0 \leq AOI \leq 90}. 
#'     The former condition assures the sun is above the horizon and 
#'     the latter condition assures the sunlight beam is able to hit the panel
#'     (Source: Stackhouse et al., 2018).
#' 
#' @return
#' @export
#'
#' @examples
#' NA
angle_of_incidence <- function(x, array.type = "fh", suffix = TRUE, 
                               azimuth = "azimuth", zenith = "zenith", 
                               # array.tilt = "array.tilt", 
                               # array.azimuth = "array.azimuth", 
                               na.val = NA,
                               zenith.max = 90, AOI.max = 90,
                               rad = TRUE, verbose = TRUE) {
  # browser()
  array.type <- .pv_array_type(array.type)
  if (verbose) cat("   Angle of incidence (AOI), array.type: ")
  if (!suffix & length(array.type) > 1) {
    stop("Sufixes must be used for calculation of several types of array.")}
  ii <- x[[zenith]] <= zenith.max & is.finite(x[[zenith]]) # sun above horizon
  # if (is.null(array.tilt)) 
  array.tilt <- "array.tilt"
  # if (is.null(array.azimuth)) 
  array.azimuth <- "array.azimuth"
  for (i in array.type) {
    if (verbose) cat(i, " ", sep = "")
    array.tilt.i <- paste0(array.tilt, ".", i)
    array.azimuth.i <- paste0(array.azimuth, ".", i)
    array.tilt.i <- ifelse(!is.null(x[[array.tilt.i]]),
                         array.tilt.i, array.tilt)
    array.azimuth.i <- ifelse(!is.null(x[[array.azimuth.i]]),
                            array.azimuth.i, array.azimuth)
    AOI <- rep(na.val, length(x[[azimuth]]))
    AOI[ii] <- acos(round(
      cospi(x[[zenith]][ii] / 180) * cospi(x[[array.tilt.i]][ii] / 180) +
        sinpi(x[[zenith]][ii] / 180) * sinpi(x[[array.tilt.i]][ii] / 180) *
        cospi((x[[azimuth]][ii] - x[[array.azimuth.i]][ii]) / 180),
      digits = 12
    ))
    stopifnot(all(!is.nan(AOI)))
    AOI[AOI > deg2rad(AOI.max)] <- na.val
    if (suffix) {
      AOI.i <- paste0("AOI.", i)
    } else {
      AOI.i <- "AOI"
    }
    x[[AOI.i]] <- AOI
  }
  if (verbose) cat("\n")
  return(x)
}

#' @rdname angle_of_incidence
#' @export
fAOI <- angle_of_incidence

if (F) {
  z0 <- pv_array_position(z)
  z1 <- angle_of_incidence(z0)
  summary(z1$AOI.fl)
  summary(z1$array.tilt.fl)
  summary(z1$array.azimuth.fl)
  
}

#' Plane-Of-Array (POA) isotropic irradiance model
#'
#' @param AOI Angle of Incidence, degrees
#' @param GHI Global Horizontal Irradiance (\mjseqn{W/m^2})
#' @param DNI Direct Normal Irradiance (\mjseqn{W/m^2})
#' @param DHI Diffuse Horizontal Irradiance (\mjseqn{W/m^2})
#' @param albedo ground-reflected portion of the POA irradiance (\mjseqn{W/m^2})
#' @param array.tilt the PV tilt angle, degrees
#'
#' @details 
#' \loadmathjax
#'   \mjsdeqn{I_{POA} = I_{POA,b} + I_{POA,d} + I_{POA,g}}
#' where:
#'     \itemize{
#'       \item \mjseqn{I_{POA} \textrm{ - the plane-of-array irradiance } (W/m^2)}
#'       \item \mjseqn{I_{POA,b} \textrm{ - the beam irradiance that hits the array } (W/m^2)}
#'         \mjsdeqn{I_{POA,b} = DNI\times\cos{(AOI)}}
#'       \item \mjseqn{I_{POA,d} \textrm{ - the sky-diffuse portion of the POA irradiance } (W/m^2)}
#'         \mjsdeqn{I_{POA,d} = DHI\times\frac{1+\cos{(array.tilt)}}{2}}
#'       \item \mjseqn{I_{POA,g} \textrm{ - the ground-reflected portion of the POA irradiance } (W/m^2)}
#'         \mjsdeqn{I_{POA,g} = GHI\times{albedo}\times\frac{1-\cos{(array.tilt)}}{2}}
#' 
#' }
#' @return
#' @export
#' 
#' @examples
#' NA
poa_irradiance <- function(x, array.type = "fl", suffix = TRUE, 
                           AOI = "AOI", GHI = "SWGDN", DNI = "DNI", 
                           DHI = "DHI", ALBEDO = "ALBEDO", 
                           array.tilt = "array.tilt", 
                           keep.all = FALSE, verbose = TRUE) {
  # browser()
  array.type <- .pv_array_type(array.type)
  if (verbose) cat("   Plane of Array Irradiance (POA), array.type: ")
  # ii <- x[[zenith]] <= zenith.max # sun above horizon
  if (!suffix & length(array.type) > 1) {
    stop("Sufixes must be used for calculation of several types of tracking.")}
  # if (is.null(AOI)) AOI <- "AOI"
  for (i in array.type) {
    if (verbose) cat(i, " ", sep = "")
    if (suffix) sfx <- paste0(".", i) else sfx <- ""
    # direct beam 
    AOI.i <- paste0(AOI, sfx)
    array.tilt.i <- paste0(array.tilt, sfx)
    y <- data.table(Eb = x[[DNI]] * cos(x[[AOI.i]]))
    names(y) <- paste0("POAb", sfx)
    # ground reflected
    y[[paste0("POAg", sfx)]] <- x[[GHI]] * x[[ALBEDO]] * 
      (1 - cospi(x[[array.tilt.i]] / 180)) / 2
    # The isotropic sky diffuse model
    y[[paste0("POAd", sfx)]] <- x[[DHI]] * 
      (1 + cospi(x[[array.tilt.i]] / 180)) / 2
    
    if (keep.all) {for (j in names(y)) {x[[j]] <- y[[j]]}}
    x[[paste0("POA", sfx)]] <- rowSums(y, na.rm = T)
  }
  if (verbose) cat("\n")
  return(x)
}

#' @param x 
#' @param array.type 
#' @param suffix 
#' @param datetime 
#' @param yday 
#' @param hour 
#' @param lon 
#' @param lat 
#' @param keep.all 
#' @param verbose 
#'
#' @rdname poa_irradiance
#' @export
fPOA <- function(x, array.type = "all",
                 suffix = TRUE, 
                 datetime = "datetime",
                 yday = "yday", hour = "hour", 
                 lon = "lon", lat = "lat",
                 keep.all = FALSE, verbose = TRUE) {
  if (verbose) {
    cat("nrow(x) = ", nrow(x),"\n")
    cat("Calculating:\n")
  }
  y <- solar_position(
    x = x, datetime = datetime, yday = yday, hour = hour,
    lon = lon, lat = lat, keep.all = FALSE, verbose = verbose) 
  y <- solar_irradiance(x = y, yday = yday, keep.all = keep.all)
  y <- pv_array_position(x = y, array.type = array.type, 
                         lat = lat, suffix = suffix)
  y <- angle_of_incidence(x = y, array.type = array.type, 
                          suffix = suffix) 
  y <- poa_irradiance(x = y, array.type = array.type,
                      suffix = suffix, verbose = verbose)
  nms <- names(y)
  if (!keep.all) nms <- nms[grepl("POA", nms)]
  for (i in nms) {
    x[[i]] <- y[[i]]
  }
  return(x)
}

# fPOA_fx 




#' Convert degrees to radians
#'
#' @param x numeric vector, degrees
#'
#' @return numeric vector, radians
#' @export
#'
#' @examples
#' rad2deg(pi)
#' deg2rad(180)
#' deg2rad(rad2deg(pi))
#' cos(pi); cos(deg2rad(rad2deg(pi)))
deg2rad <- function(x) {
  x * pi / 180
}

#' @rdname deg2rad
#' @export
rad2deg <- function(x) x * 180 / pi

if (F) {
  library(tidyverse)
  library(data.table)
  size <- energyRt::size
  x <- merra2_dec %>%
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
  y <- solar_position(x)
  z <- solar_irradiance(y, keep.all = T) #, zenith_max = 90
  # summary(z$DNI); summary(z$DHI)
  # z0 <- pv_array_position(z, array.type = "fl")
  # z0 <- pv_array_position(z0, array.type = "th")
  # z0 <- pv_array_position(z0, array.type = "tl")
  # z0 <- pv_array_position(z0, array.type = "tv")
  # z0 <- pv_array_position(z0, array.type = "td")
  z <- pv_array_position(z, array.type = "all")
  # z1 <- angle_of_incidence(z0)
  # z1 <- angle_of_incidence(z1, "th")
  z <- angle_of_incidence(z, "all")
  # summary(z$AOI.fl); summary(z$AOI.th)
  # summary(select(z, starts_with("AOI")))
  z <- poa_irradiance(z, array.type = "all", keep.all = T)
  # z2 <- poa_irradiance(z2, array.type = "th", keep.all = T)
  # z2
  # summary(z2$POAb.fl)
  # summary(z2$POA.fl)
  # summary(z2$POA.th)
  # z
  # summary(select(z, starts_with("POA.")))
  sapply(select(z, starts_with("POA.")), sum, na.rm = T)/1e6
  
  # z1$array.azimuth.fl <- 180
#   poa_irradiance(z1[1:10,], keep.all = T)
#   poa_irradiance(z1, keep.all = T)
}
