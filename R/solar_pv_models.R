.pv_track_types <- function(type) {
  .tracking_types <- c("00", "fx", "hz", "tl", "vt", "dl")
  if (length(type) == 1) {
    if (grepl("ALL", type, ignore.case = T)) {type <- .tracking_types}
  } else if (is.null(type)) {
    type <- .tracking_types
  }
  return(type)
}


#' Photovoltaic Solar Panel Orientation and Performance Models
#'
#' @param lon longitude of PV location (\mjseqn{-180 \leq lon \leq 180})
#' @param lat latitude of PV location (\mjseqn{-90 \leq lat \leq 90})
#' @param azimuth solar azimuth angle for the PV location (\mjseqn{0 \leq azimuth < 360})
#' @param zenith solar zenith angle for the PV location (\mjseqn{0 \leq azimuth \leq 90})
#' @param x 
#' @param type 
#' @param add.suffix 
#' @param array.tilt.range.fx 
#' @param array.tilt.range.hz 
#' @param array.tilt.range.tl 
#' @param array.tilt.range.vt 
#' @param array.tilt.range.dl 
#'
#' @details 
#' \loadmathjax
#' \describe{
#'   \item{Fixed PV Panel (\mjseqn{*.fx})}
#'        {South-facing fixed solar PV with the tilted angle equal to the site's latitude.}
#'   
#'     \itemize{
#'       \item PV Tilted Angle (\mjseqn{array.tilt}, in degrees)
#'         \mjsdeqn{array.tilt = latitude}
#'       \item PV Azimuth Angle (\mjseqn{array.azimuth}, in degrees)
#'         \mjsdeqn{array.azimuth = 180}
#'     }
#'   
#'   \item{Horizontal Single-Axis PV Tracker (\mjseqn{*.hz})}
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
#'   \item{Vertical Single-Axis Tracker (\mjseqn{*.vt})}
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
#'   \item{Dual-Axis Tracker (\mjseqn{*.dl})}
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
solar_pv_position <- function(x, type = "fx", add.suffix = TRUE,
                              lon = NULL, lat = NULL, 
                              azimuth = NULL, zenith = NULL,
                              # zenith.max = 
                              array.tilt.range.00 = c(0., 0.),
                              array.tilt.range.fx = c(0., 60.),
                              array.tilt.range.hz = c(0., 45.),
                              array.tilt.range.tl = array.tilt.range.hz,
                              array.tilt.range.vt = array.tilt.range.fx,
                              array.tilt.range.dl = array.tilt.range.hz) {
  # browser()
  type <- .pv_track_types(type)
  ii <- x$zenith <= 90
  for (i in type) {
    y <- data.table(
      array.tilt = as.numeric(rep(NA, nrow(x))),
      array.azimuth = as.numeric(rep(NA, nrow(x)))
    )
    # browser()
    if (i == "00") { 
      y$array.azimuth <- 0 # Southern hemisphere facing North
      # y$array.azimuth[x$lat > 0] <- 180 # Northern hemisphere facing South
      y$array.tilt <- 0
      y$array.tilt[y$array.tilt < array.tilt.range.00[1]] <- array.tilt.range.00[1]
      y$array.tilt[y$array.tilt > array.tilt.range.00[2]] <- array.tilt.range.00[2]
    } else if (i == "fx") { 
      # fixed tilted ####
      y$array.azimuth <- 0 # Southern hemisphere facing North
      y$array.azimuth[x$lat > 0] <- 180 # Northern hemisphere facing South
      y$array.tilt <- abs(x$lat)
      y$array.tilt[y$array.tilt < array.tilt.range.fx[1]] <- array.tilt.range.fx[1]
      y$array.tilt[y$array.tilt > array.tilt.range.fx[2]] <- array.tilt.range.fx[2]
    } else if (i == "hz") { 
      # horizontal tracking ####
      y$array.azimuth <- 90 + as.numeric(x$azimuth > 180) * 180
      # ii <- x$zenith < 90
      y$array.tilt[ii] <- 
        atan(abs(
          tan(x$zenith[ii] / 180 * pi) * 
            cospi((x$azimuth[ii] - y$array.azimuth[ii]) / 180)
          )) / pi * 180 
      y$array.tilt[!ii] <- 90
      y$array.tilt[y$array.tilt < array.tilt.range.hz[1]] <- array.tilt.range.hz[1]
      y$array.tilt[y$array.tilt > array.tilt.range.hz[2]] <- array.tilt.range.hz[2]
      
    } else if (i == "tl") { 
      # horizontal tilted tracking ####
      # browser()
      y$array.tilt <- abs(x$lat)
      # y$array.azimuth <- 180
      y$array.azimuth <- 0 # Southern hemisphere facing North
      y$array.azimuth[x$lat > 0] <- 180 # Northern hemisphere facing South
      if (is.null(x[["AOI.fx"]])) {
        y$zenith <- x$zenith
        y$azimuth <- x$azimuth
        y <- angle_of_incidence(y, type = "fx")
        y$zenith <- NULL; y$azimuth <- NULL
      } else {
        y[["AOI.fx"]] <- x[["AOI.fx"]]
      }
      y$delta.gamma <- atan(sinpi(x$zenith / 180) *
                            sinpi((x$azimuth - 180) / 180) / 
                              cos(y$AOI.fx) / sinpi(x$lat / 180)) / pi * 180
      y$AOI.fx <- NULL
      # ii <- zenith <= 90
      y$array.azimuth <- 0.
      y$array.azimuth[ii] <- 180 + y$delta.gamma[ii] +
        ((y$delta.gamma[ii] * (x$azimuth[ii] - 180)) < 0) * 
        (2 * ((x$azimuth[ii] - 180) >= 0) - 1) * 180
      y$delta.gamma <- NULL
      y$array.tilt <- array.tilt.range.tl[2]
      y$array.tilt[ii] <- (atan(tan(x$zenith[ii] / 180 * pi) / 
                                  cospi((y$array.azimuth[ii] - 180) / 180)) +
                       (cospi(y$array.azimuth[ii] / 180 - 1) < 0) * pi) / pi * 180
      y$array.tilt[y$array.tilt < array.tilt.range.tl[1]] <- array.tilt.range.tl[1]
      y$array.tilt[y$array.tilt > array.tilt.range.tl[2]] <- array.tilt.range.tl[2]

    } else if (i == "vt") { 
      # vertical (azimuth) tracking ####
      y$array.tilt <- abs(x$lat)
      y$array.azimuth <- x$azimuth
      y$array.tilt[y$array.tilt < array.tilt.range.vt[1]] <- array.tilt.range.vt[1]
      y$array.tilt[y$array.tilt > array.tilt.range.vt[2]] <- array.tilt.range.vt[2]
      
    } else if (i == "dl") { 
      # dual axes tracking ####
      y$array.tilt <- array.tilt.range.dl[2]
      y$array.tilt[ii] <- x$zenith[ii]
      y$array.tilt[y$array.tilt < array.tilt.range.dl[1]] <- array.tilt.range.dl[1]
      y$array.tilt[y$array.tilt > array.tilt.range.dl[2]] <- array.tilt.range.dl[2]
      y$array.azimuth <- x$azimuth
      
    } else {
      stop("Unknown type of PV tracking system '", i, "'.")
    }
    # y -> x
    array.tilt <- "array.tilt"; array.azimuth <- "array.azimuth"
    if (add.suffix) {
      array.tilt <- paste0(array.tilt, ".", i)
      array.azimuth <- paste0(array.azimuth, ".", i)
      names(y) <- c(array.tilt, array.azimuth)
    }
    x[[array.tilt]] <- y$array.tilt
    x[[array.azimuth]] <- y$array.azimuth
    
  }
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
angle_of_incidence <- function(x, type = "fx", add.suffix = TRUE, 
                               azimuth = NULL, zenith = NULL, 
                               array.tilt = NULL, array.azimuth = NULL, 
                               na.val = NA, 
                               zenith.max = 90, AOI.max = 90,
                               rad = TRUE) {
  # browser()
  type <- .pv_track_types(type)
  if (!add.suffix & length(type) > 1) {
    stop("Sufixes must be used for calculation of several types of tracking.")}
  ii <- x$zenith <= zenith.max # sun above horizon
  if (is.null(array.tilt)) array.tilt <- "array.tilt"
  if (is.null(array.azimuth)) array.azimuth <- "array.azimuth"
  for (i in type) {
    array.tilt.i <- paste0(array.tilt, ".", i)
    array.azimuth.i <- paste0(array.azimuth, ".", i)
    array.tilt.i <- ifelse(!is.null(x[[array.tilt.i]]), 
                         array.tilt.i, array.tilt)
    array.azimuth.i <- ifelse(!is.null(x[[array.azimuth.i]]), 
                            array.azimuth.i, array.azimuth)
    AOI <- rep(na.val, length(x$azimuth))
    AOI[ii] <- acos(round(
      cospi(x$zenith[ii] / 180) * cospi(x[[array.tilt.i]][ii] / 180) +
        sinpi(x$zenith[ii] / 180) * sinpi(x[[array.tilt.i]][ii] / 180) *
        cospi((x$azimuth[ii] - x[[array.azimuth.i]][ii]) / 180),
      digits = 15
    ))
    AOI[AOI > deg2rad(AOI.max)] <- na.val
    if (add.suffix) {
      AOI.i <- paste0("AOI.", i)
    } else {
      AOI.i <- "AOI"
    }
    x[[AOI.i]] <- AOI
  }
  return(x)
}

#' @rdname angle_of_incidence
#' @export
fAOI <- angle_of_incidence

if (F) {
  z0 <- solar_pv_position(z)
  z1 <- angle_of_incidence(z0)
  summary(z1$AOI.fx)
  summary(z1$array.tilt.fx)
  summary(z1$array.azimuth.fx)
  
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
plane_of_array <- function(x, type = "fx", add.suffix = TRUE, 
                           AOI = NULL, GHI = NULL, DNI = NULL, 
                           DHI = NULL, ALBEDO = NULL, 
                           array.tilt = NULL, ALL = FALSE) {
  # browser()
  type <- .pv_track_types(type)
  # ii <- x$zenith <= zenith.max # sun above horizon
  if (!add.suffix & length(type) > 1) {
    stop("Sufixes must be used for calculation of several types of tracking.")}
  if (is.null(AOI)) AOI <- "AOI"
  for (i in type) {
    if (add.suffix) sfx <- paste0(".", i) else sfx <- ""
    # direct beam 
    AOI.i <- paste0(AOI, sfx)
    array.tilt <- paste0("array.tilt", sfx)
    y <- data.table(Eb = x$DNI * cos(x[[AOI.i]]))
    names(y) <- paste0("POAb", sfx)
    # ground reflected
    y[[paste0("POAg", sfx)]] <- x$SWGDN * x$ALBEDO * 
      (1 - cospi(x[[array.tilt]] / 180)) / 2
    # The isotropic sky diffuse model
    y[[paste0("POAd", sfx)]] <- x$DHI * (1 + cospi(x[[array.tilt]] / 180)) / 2
    
    if (ALL) {for (j in names(y)) {x[[j]] <- y[[j]]}}
    x[[paste0("POA", sfx)]] <- rowSums(y, na.rm = T)
  }
  return(x)
}

#' @rdname plane_of_array
#' @export
fPOA <- plane_of_array

fPOA_fx 




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
  z <- solar_irradiance(y, ALL = T) #, zenith_max = 90
  # summary(z$DNI); summary(z$DHI)
  # z0 <- solar_pv_position(z, type = "fx")
  # z0 <- solar_pv_position(z0, type = "hz")
  # z0 <- solar_pv_position(z0, type = "tl")
  # z0 <- solar_pv_position(z0, type = "vt")
  # z0 <- solar_pv_position(z0, type = "dl")
  z <- solar_pv_position(z, type = "all")
  # z1 <- angle_of_incidence(z0)
  # z1 <- angle_of_incidence(z1, "hz")
  z <- angle_of_incidence(z, "all")
  # summary(z$AOI.fx); summary(z$AOI.hz)
  # summary(select(z, starts_with("AOI")))
  z <- plane_of_array(z, type = "all", ALL = T)
  # z2 <- plane_of_array(z2, type = "hz", ALL = T)
  # z2
  # summary(z2$POAb.fx)
  # summary(z2$POA.fx)
  # summary(z2$POA.hz)
  # z
  # summary(select(z, starts_with("POA.")))
  sapply(select(z, starts_with("POA.")), sum, na.rm = T)/1e6
  
  # z1$array.azimuth.fx <- 180
#   plane_of_array(z1[1:10,], ALL = T)
#   plane_of_array(z1, ALL = T)
}
