.pv_array_type <- function(array.type, asFactor = FALSE) {
  # internal function
  .tracking_types <- c("fh", "fl", "th", "tv", "tl", "td")
  # "fh" # "Horizontal (h) fixed (f) arrays"
  # "fl" # "Tilted (l) fixed (f) arrays"
  # "th" # "Horizontal (h) single axis tracking (t) arrays"
  # "tv" # "Vertical (v) single axis tracking (t) arrays"
  # "tl" # "Tilted (l) single axis tracking (t) arrays" - testing
  # "td" # "Dual (d) axis tracking (t) arrays"
  
   if (length(array.type) == 1) {
    if (grepl("ALL", array.type, ignore.case = T)) {array.type <- .tracking_types}
  } else if (is.null(array.type)) {
    array.type <- .tracking_types
  }
  if (asFactor) array.type <- factor(array.type, levels = .tracking_types, ordered = TRUE)
  return(array.type)
}

#' List tracking system types
#'
#' @return
#' @export
#'
#' @examples
#' pv_array_types()
#' pv_array_types("fl")
pv_array_types <- function(array.type = "all", asFactor = FALSE) {
  d <- data.frame(
    array.type = .pv_array_type(array.type, asFactor = asFactor),
    description = ""
  )
  d$description[d$array.type == "fh"] <- "Fixed (f) horizontal (h)"
  d$description[d$array.type == "fl"] <- "Fixed (f) tilted (l)"
  d$description[d$array.type == "th"] <- "Single axis horizontal (h) tracking (t)"
  d$description[d$array.type == "tv"] <- "Single axis vertical (v) tracking (t)"
  d$description[d$array.type == "tl"] <- "Single axis tilted (l) tracking (t)" # - testing
  d$description[d$array.type == "td"] <- "Dual (d) axis tracking (t)"
  return(d)
}

#' Photovoltaic Solar Panel Orientation and Performance Models
#'
#' @param x data.frame object with MERRA-2 subset
#' @param lat latitude of PV location (\mjseqn{-90 \leq lat \leq 90})
#' @param azimuth_Q solar azimuth angle for the PV location (\mjseqn{0 \leq azimuth < 360})
#' @param zenith solar zenith angle for the PV location (\mjseqn{0 \leq azimuth \leq 90})
#' @param array.type type of tracking ()
#' @param verbose 
#' @param tilt.param 
#' @param suffix 
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
                              azimuth_Q = "azimuth_Q", zenith = "zenith",
                              verbose = getOption("merra2.verbose"),
                              tilt.param = tilt.param.default()
                              ) {
  # browser()
  # c("fh", "fl", "th", "tl", "tv", "td")
  array.type <- .pv_array_type(array.type)
  if (verbose) cat("   PV-array position, array.type: ")
  ii <- x[[zenith]] <= 90 & is.finite(x[[zenith]]) # sun over horizon
  # south <- x[[lat]] < 0
  for (i in array.type) {
    if (verbose) cat(i, " ", sep = "")
    y <- data.table(
      array.tilt = as.numeric(rep(NA, nrow(x))),
      array.azimuth_Q = as.numeric(rep(NA, nrow(x)))
    )
    if (i == "fh") { 
      # fixed horizontal ####
#<<<<<<< tl_tracking
      # y$array.azimuth <- 0 # Southern hemisphere facing North
      # y$array.azimuth[x[[lat]] > 0] <- 180 # Northern hemisphere facing South
      y$array.azimuth_Q <- 0 # facing equator
#=======
#      y$array.azimuth <- 0 # Southern hemisphere facing North
#      y$array.azimuth[x[[lat]] > 0] <- 180 # Northern hemisphere facing South
      # y$array.azimuth <- 0 # facing equator
#>>>>>>> master
      y$array.tilt <- 0
      # y$array.tilt[y$array.tilt < array.tilt.range.fh[1]] <- array.tilt.range.fh[1]
      # y$array.tilt[y$array.tilt > array.tilt.range.fh[2]] <- array.tilt.range.fh[2]
      
    } else if (i == "fl") { 
      # fixed tilted ####
#<<<<<<< tl_tracking
      # y$array.azimuth <- 0 # Southern hemisphere facing North
      # y$array.azimuth[x[[lat]] > 0] <- 180 # Northern hemisphere facing South
      y$array.azimuth_Q <- 0 # facing equator
#=======
#      y$array.azimuth <- 0 # Southern hemisphere facing North
#      y$array.azimuth[x[[lat]] > 0] <- 180 # Northern hemisphere facing South
      # y$array.azimuth <- 0 # facing equator
#>>>>>>> master
      y$array.tilt <- abs(x[[lat]])
      # y$array.tilt[y$array.tilt < array.tilt.range.fl[1]] <- array.tilt.range.fl[1]
      # y$array.tilt[y$array.tilt > array.tilt.range.fl[2]] <- array.tilt.range.fl[2]
    } else if (i == "th") { 
      # tracking horizontal ####
#<<<<<<< tl_tracking
      # y$array.azimuth <- 90 + as.numeric(x[[azimuth]] > 180) * 180
      y$array.azimuth_Q <- -90 + as.numeric(x[[azimuth_Q]] >= 0) * 180
#=======
#      y$array.azimuth <- 90 + as.numeric(x[[azimuth]] > 180) * 180
      # y$array.azimuth <- -90 + as.numeric(x[[azimuth]] >= 0) * 180
#>>>>>>> master
      # ii <- x[[zenith]] < 90
      y$array.tilt[ii] <- 
        atan(abs(
          tan(x[[zenith]][ii] / 180 * pi) * 
            cosd(x[[azimuth_Q]][ii] - y$array.azimuth_Q[ii])
          )) / pi * 180 
      y$array.tilt[!ii] <- 0
      # y$array.tilt[y$zenith > array.tilt.range.th[3]] <- 0
      # y$array.tilt[y$array.tilt < array.tilt.range.th[1]] <- array.tilt.range.th[1]
      # y$array.tilt[y$array.tilt > array.tilt.range.th[2]] <- array.tilt.range.th[2]
      # y$array.tilt[y$array.tilt > array.tilt.range.th[4]] <- 0
    } else if (i == "tl") { 
      # tracking tilted ####
      # browser()
      y$array.tilt <- abs(x[[lat]])
      y$array.tilt[y$array.tilt == 0] <- 1e-10 # workaround for lat == 0 (equator)
      y$array.tilt[y$array.tilt < tilt.param$tl$min] <- tilt.param$tl$min
      y$array.tilt[y$array.tilt > tilt.param$tl$max] <- tilt.param$tl$max
      y$array.azimuth_Q <- 0 # facing equator
      y[[zenith]] <- as.numeric(NA)
      y[[azimuth_Q]] <- as.numeric(NA)
      y[[zenith]][ii] <- x[[zenith]][ii]
      y[[azimuth_Q]][ii] <- x[[azimuth_Q]][ii]
      AOI.fl <- acos(
        # round(
        cosd(y[[zenith]][ii]) * cosd(y$array.tilt[ii]) + 
          sind(y[[zenith]][ii]) * sind(y$array.tilt[ii]) * 
          cosd(y[[azimuth_Q]][ii] - y$array.azimuth_Q[ii])
        # , digits = 15)
      )
      
      delta.gamma <- atan(
#<<<<<<< tl_tracking
        # sind(y[[zenith]][ii]) * sind((y[[azimuth]][ii] - 180)) /
        sind(y[[zenith]][ii]) * sind((y[[azimuth_Q]][ii] - 0)) /
#=======
        # sind(y[[zenith]][ii]) * sind((y[[azimuth]][ii] - 0)) /
#        sind(y[[zenith]][ii]) * sind((y[[azimuth]][ii] - 180)) /
#>>>>>>> master
          (cos(AOI.fl) * sind(y$array.tilt[ii]))
        ) / pi * 180
      # rm(array.azimuth,array.tilt,AOI.fx)
      
      y$array.azimuth_Q[ii] <- 
#<<<<<<< tl_tracking
        # (180 + delta.gamma + ((delta.gamma * (y[[azimuth]][ii] - 180)) < 0) * 
        #    (2*((y[[azimuth]][ii] - 180) >= 0) - 1) * 180) #* (zenith < 90)        
        (delta.gamma + ((delta.gamma * y[[azimuth_Q]][ii]) < 0) * 
           (2*(y[[azimuth_Q]][ii] >= 0) - 1) * 180) #* (zenith < 90)
#=======
#        (180 + delta.gamma + ((delta.gamma * (y[[azimuth]][ii] - 180)) < 0) * 
#           (2*((y[[azimuth]][ii] - 180) >= 0) - 1) * 180) #* (zenith < 90)        
        # (delta.gamma + ((delta.gamma * y[[azimuth]][ii]) < 0) * 
        #    (2*(y[[azimuth]][ii] >= 0) - 1) * 180) #* (zenith < 90)
#>>>>>>> master
      # rm(delta.gamma, AOI.fl)
      # cbind(y, delta.gamma, AOI.fl)
      # gc()
      y$array.tilt[ii] <- (
        atan(
#<<<<<<< tl_tracking
          tand(y$array.tilt[ii]) / cosd(y$array.azimuth_Q[ii])) + 
          (cosd(y$array.azimuth_Q[ii]) < 0) * pi) / pi * 180 
          # tand(y[[zenith]][ii]) / cosd((y[[azimuth]][ii] - 180))) + 
          # (cosd(y[[azimuth]][ii] - 180) < 0) * pi) / pi * 180           
#=======
#          tand(y[[zenith]][ii]) / cosd((y[[azimuth]][ii] - 180))) + 
#          (cosd(y[[azimuth]][ii] - 180) < 0) * pi) / pi * 180           
          # tand(y$array.tilt[ii]) / cosd(y$array.azimuth[ii])) + 
          # (cosd(y$array.azimuth[ii]) < 0) * pi) / pi * 180 
#>>>>>>> master
      y$array.tilt[!ii] <- 0
      
      y[[zenith]] <- NULL; y[[azimuth_Q]] <- NULL
      # # browser()
      # # y$array.tilt <- as.numeric(0)
      # # `iii` - handle cases when zenith > 90 & GHI > 0
      # cospi.array.az <- rep(0., length(ii))
      # cospi.array.az[ii] <- round(cospi((y$array.azimuth[ii] - 180) / 180), 5)
      # iii <- ii & (abs(cospi.array.az) > 0)
      # y$array.tilt[iii] <- (atan(tanpi(x[[zenith]][iii] / 180) / 
      #                              cospi.array.az[iii]) +
      #                  (cospi.array.az[iii] < 0) * pi) / pi * 180
      # # southern hemisphere adjustment
      # tilt_180 <- y$array.tilt > 90 & !is.na(y$array.tilt)
      # y$array.tilt[tilt_180] <- 180 - y$array.tilt[tilt_180]
      # apply boundaries
      # y$array.tilt[y$array.tilt < array.tilt.range.tl[1]] <- array.tilt.range.tl[1]
      # y$array.tilt[y$array.tilt > array.tilt.range.tl[2]] <- array.tilt.range.tl[2]

    } else if (i == "tv") { 
      # tracking vertical (azimuth) ####
      # browser()
      y$array.tilt <- abs(x[[lat]])
      y$array.azimuth_Q <- x[[azimuth_Q]]
      # y$array.tilt[y$array.tilt < array.tilt.range.tv[1]] <- array.tilt.range.tv[1]
      # y$array.tilt[y$array.tilt > array.tilt.range.tv[2]] <- array.tilt.range.tv[2]
      
    } else if (i == "td") { 
      # tracking dual axes ####
      y$array.tilt <- 0
      y$array.tilt[ii] <- x[[zenith]][ii]
      # y$array.tilt[x[[zenith]] > array.tilt.range.td[3]] <- 0
      # y$array.tilt[y$array.tilt < array.tilt.range.td[1]] <- array.tilt.range.td[1]
      # y$array.tilt[y$array.tilt > array.tilt.range.td[2]] <- array.tilt.range.td[2]
      y$array.azimuth_Q <- x[[azimuth_Q]]
      
    } else {
      stop("Unknown array.type '", i, "'.")
    }
    if (tilt.param[[i]]$backtracking) {
      y$array.tilt[x[[zenith]] > tilt.param[[i]]$shading] <- 0
    }
    y$array.tilt[y$array.tilt < tilt.param[[i]]$min] <- tilt.param[[i]]$min
    y$array.tilt[y$array.tilt > tilt.param[[i]]$max] <- tilt.param[[i]]$max
    # y -> x
    array.tilt <- "array.tilt"; array.azimuth_Q <- "array.azimuth_Q"
    if (suffix) {
      array.tilt <- paste0(array.tilt, ".", i)
      array.azimuth_Q <- paste0(array.azimuth_Q, ".", i)
      names(y) <- c(array.tilt, array.azimuth_Q)
    }
    x[[array.tilt]] <- y$array.tilt
    x[[array.azimuth_Q]] <- y$array.azimuth_Q
    
  }
  if (verbose) cat("\n")
  return(x)
}


#' Default tilt-parameters of tracking systems
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
#' str(tilt.param.default())
tilt.param.default <- function(x = NULL) {
  list(
    fh = list(min = 0, max = 0, shading = 90, backtracking = FALSE),
    fl = list(min = 0, max = 60, shading = 89, backtracking = FALSE),
    th = list(min = 0, max = 60, shading = 89, backtracking = TRUE),
    tl = list(min = 0, max = 60, shading = 89, backtracking = TRUE),
    tv = list(min = 0, max = 60, shading = 89, backtracking = FALSE),
    td = list(min = 0, max = 60, shading = 89, backtracking = TRUE)
  )
}

#' Angle of Incidence (AOI)
#'
#' @param x 
#' @param azimuth_Q the solar zenith angle, degrees
#' @param array.type 
#' @param suffix 
#' @param na.val 
#' @param zenith.max 
#' @param verbose 
#' @param zenith the solar azimuth angle, degrees
#' @param ... 
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
                               azimuth_Q = "azimuth_Q", zenith = "zenith", 
                               # beam = "beam",
                               na.val = NA,
                               zenith.max = 90, AOI.max = 90,
                               verbose = getOption("merra2.verbose"),
                               ...) {
  # browser()
  array.type <- .pv_array_type(array.type)
  if (verbose) cat("   Angle of incidence (AOI), array.type: ")
  if (!suffix & length(array.type) > 1) {
    stop("Sufixes must be used for calculation of several types of array.")}
  ii <- x[[zenith]] <= zenith.max & is.finite(x[[zenith]]) # additional filter
  # if (!is.null(x[[beam]])) ii <- ii & x[[beam]]
  # if (is.null(array.tilt)) 
  array.tilt <- "array.tilt"
  # if (is.null(array.azimuth)) 
  array.azimuth_Q <- "array.azimuth_Q"
  for (i in array.type) {
    if (verbose) cat(i, " ", sep = "")
    array.tilt.i <- paste0(array.tilt, ".", i)
    array.azimuth_Q.i <- paste0(array.azimuth_Q, ".", i)
    array.tilt.i <- ifelse(!is.null(x[[array.tilt.i]]),
                         array.tilt.i, array.tilt)
    array.azimuth_Q.i <- ifelse(!is.null(x[[array.azimuth_Q.i]]),
                            array.azimuth_Q.i, array.azimuth_Q)
    AOI <- rep(na.val, length(x[[azimuth_Q]]))
    AOI[ii] <- 
      # acos(
      # round(
      cosd(x[[zenith]][ii]) * cosd(x[[array.tilt.i]][ii]) +
        sind(x[[zenith]][ii]) * sind(x[[array.tilt.i]][ii]) *
        cosd((x[[azimuth_Q]][ii] - x[[array.azimuth_Q.i]][ii]))
      # digits = 12)
    # )
    AOI[ii][AOI[ii] > 1] <- 1
    AOI[ii][AOI[ii] < -1] <- -1
    AOI[ii] <- acos(AOI[ii])
    stopifnot(all(!is.nan(AOI)))
    AOI[AOI > deg2rad(AOI.max)] <- na.val
    if (suffix) {
      AOI.i <- paste0("AOI.", i)
    } else {
      AOI.i <- "AOI"
    }
    x[[AOI.i]] <- AOI
    # AOI.i.d <- paste0(AOI.i, ".degree")
    # browser()
    # x[[AOI.i.d]] <- rad2deg(AOI)
  }
  if (verbose) cat("\n")
  return(x)
}

#' @rdname angle_of_incidence
#' @export
fAOI <- angle_of_incidence

if (F) {
  x <- merra2_mar %>%
    add_coord() %>%
    filter(lon == 0, lat %in% c(-80, -45, -10, -.5, 0, .5, 10, 45, 80), hour(UTC) == 9) %>%
    solar_position(keep.all = T)
  
  x %>% pv_array_position(array.type = "all")
  
  
  z0 <- pv_array_position(z)
  z1 <- angle_of_incidence(z0)
  summary(z1$AOI.fl)
  summary(z1$array.tilt.fl)
  summary(z1$array.azimuth_Q.fl)
  
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
                           zenith = "zenith", 
                           tilt.param = tilt.param.default(),
                           keep.all = FALSE, verbose = getOption("merra2.verbose"),
                           ...) {
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
    if (!tilt.param[[i]]$backtracking) {
      # no direct beam when sun is lower than "shading angle"
      y$Eb[x[[zenith]] > tilt.param[[i]]$shading] <- 0
    }
    names(y) <- paste0("POAb", sfx)
    # ground reflected
    y[[paste0("POAg", sfx)]] <- x[[GHI]] * x[[ALBEDO]] * 
      (1 - cosd(x[[array.tilt.i]])) / 2
    # The isotropic sky diffuse model
    y[[paste0("POAd", sfx)]] <- x[[DHI]] * 
      (1 + cosd(x[[array.tilt.i]])) / 2
    
    if (keep.all) {for (j in names(y)) {x[[j]] <- y[[j]]}}
    x[[paste0("POA", sfx)]] <- rowSums(y, na.rm = T)
  }
  if (verbose) cat("\n")
  return(x)
}

#' @param x 
#' @param array.type 
#' @param suffix 
#' @param UTC 
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
                 UTC = "UTC",
                 yday = "yday", hour = "hour", 
                 lon = "lon", lat = "lat",
                 integral_steps = 1,
                 tilt.param = tilt.param.default(),
                 keep.all = FALSE, verbose = getOption("merra2.verbose")) {
  # browser()
  if (verbose) {
    cat("nrow(x) = ", nrow(x),"\n")
  }
  if (is.null(x[["locid"]])) {
    if (is.null(x[[lon]] || is.null(x[[lat]]))) {
      stop("'x' should have either coordinates ('", 
           lon ,"' and '", lat, "') or 'locid' columns")
    }
    y <- x
  } else {
    if (!is.null(x[[lon]]) & !is.null(x[[lat]])) {
      y <- x
    } else {
      if (verbose) cat("Adding 'locid' coordinates\n")
      # y[[lon]] <- NULL; y[[lat]] <- NULL
      # lid <- merra2ools::locid[,3]
      # lid[[lon]] <- merra2ools::locid[["lon"]]
      # lid[[lat]] <- merra2ools::locid[["lat"]]
      # y$locid <- as.integer(y$locid)
      # y <- dplyr::right_join(lid, y, by = "locid")
      x <- add_coord(x)
      y <- x
      lon <- "lon"; lat <- "lat"
    }
  }
  if (verbose) cat("Calculating:\n")
  y <- solar_position(
    x = y, UTC = UTC, yday = yday, hour = hour,
    lon = lon, lat = lat, integral_steps = integral_steps,
    keep.all = TRUE, verbose = verbose) 
  # browser()
  y <- ghi_decomposition(x = y, yday = yday, keep.all = TRUE, 
                        verbose = verbose)
  y <- pv_array_position(x = y, array.type = array.type, 
                         lat = lat, suffix = suffix, 
                         verbose = verbose)
  y <- angle_of_incidence(x = y, array.type = array.type, 
                          suffix = suffix, verbose = verbose) 
  y <- poa_irradiance(x = y, array.type = array.type,
                      suffix = suffix, verbose = verbose,
                      keep.all = keep.all)
  nms <- names(y)
  nms <- nms[!(nms %in% names(x))]
  if (!keep.all) nms <- nms[grepl("POA", nms)]
  # for (i in nms) {
  #   x[[i]] <- y[[i]]
  # }
  # bind_cols()
  x <- cbind(x, dplyr::select(y, dplyr::all_of(nms)))
  return(x)
}


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
deg2rad <- function(x) {x * pi / 180}

#' @param x 
#'
#' @rdname deg2rad
#' @export
rad2deg <- function(x) {x * 180 / pi}

cosd <- function(x, check = TRUE) {
  x <- cos(x * pi / 180) # faster than cospi(x / 180) & same divergence from cos
  if (check) { #
    x[x > 1] <- 1
    x[x < -1] <- -1
  }
  return(x)
}

sind <- function(x, check = TRUE) {
  x <- sin(x * pi / 180) # faster than sinpi(x / 180) & closer to sin
  if (check) { #
    x[x > 1] <- 1
    x[x < -1] <- -1
  }
  return(x)
}

tand <- function(x, Inf.eps = 1e-12, Inf.val = NaN) {
  
  tanpi(x / 180) # same speed as tanpi(x / 180) & same divergence from tan
}

