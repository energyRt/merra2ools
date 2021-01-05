#' Irradiance decomposition
#'
#' @param yday day of a year, integer vector
#' @param SWGDN Global Horizontal Irradiance from MERRA-2 subset (\mjseqn{GHI, W/m^2})
#' @param zenith Zenith angle, degrees
#'
#' @details 
#' \loadmathjax
#' List or data.frame with estimated following solar geometry variables:
#' \itemize{
#'   \item Extraterrestrial irradiance (\mjseqn{G_e})
#'     \mjsdeqn{G_e = G_{sc}\times\big(1+0.033\cos{(\frac{360n}{365})}\big)}
#'     where: \cr
#'     \mjseqn{G_{sc} = 1360.8W/m^2}, is the solar constant based on the latest 
#'     NASA observation (Kopp and Lean, 2011); \cr
#'     \mjseqn{n - } day of the year. \cr
#'     
#'   \item Clearness index (\mjseqn{k_t})
#'     \mjsdeqn{k_t = \frac{GHI}{G_e\cos{(zenith)}}}
#'     
#'   \item Diffuse fraction (\mjseqn{k_d})
#'      \mjsdeqn{k_d = \begin{cases}
#'          1-0.09k_t & & {k_t < 0.22}\newline
#'          0.9511-0.1604k_t+4.388k_t^2-16.638k_t^3+12.336k_t^4 & & {0.22 \leq k_t \leq 0.8}\newline
#'          0.165& & {k_t > 0.8}
#'          \end{cases}
#'          }
#'          
#'   \item Direct Normal Irradiance (\mjseqn{DNI, W/m^2})
#'      \mjsdeqn{DNI = \frac{(1-k_d)}{\cos{(zenith)}}\times{GHI}}
#'          
#'   \item Diffuse Horizontal Irradiance (\mjseqn{DHI, W/m^2})
#'      \mjsdeqn{DHI = k_d\times{GHI}}
#'     where: \cr
#'     \mjseqn{GHI} - Global Horizontal Irradiance (\mjseqn{GHI, W/m^2}) from MERRA-2 dataset.
#'     \mjsdeqn{GHI = DHI + DNI \times{\cos{(zenith)}}}
#' } 
#'     
#' @return
#' 
#' @export
#' @import mathjaxr
#'
#' @examples
#' NA
solar_irradiance <- function(x, yday = "yday", GHI = "SWGDN", 
                             zenith = "zenith", zenith_max = 85,
                             keep.all = FALSE, verbose = getOption("merra2.verbose")) {
  # browser()
  # if (is.null(x)) {
  #   stopifnot(!is.null(yday))
  #   stopifnot(!is.null(GHI))
  #   stopifnot(!is.null(zenith))
  #   x <- data.table(
  #     yday = yday,
  #     GHI = GHI,
  #     zenith = zenith
  #   )
  #   rm(yday, GHI, zenith)
  # }
  stopifnot(!is.null(x[[yday]]))
  stopifnot(!is.null(x[[GHI]]))
  stopifnot(!is.null(x[[zenith]]))
  if (verbose) cat("   DNI and DHI\n")
  zz <- x[[zenith]] <= zenith_max # avoiding excessive values at horizon
  # the solar constant
  Gsc <- 1360.8
  # Extraterrestrial irradiance
  Ge <- (1 + 0.033 * cos(360 * x[[yday]] / 365)) * Gsc
  # Clearness index
  k.t <- x[[GHI]] / Ge / cospi(x[[zenith]] / 180)
  # k.t[!zz] <- 0
  k.t[k.t < 0] <- 0
  k.t[k.t > 1] <- 1
  # Diffuse fraction
  k.d <- rep(0, nrow(x))
  ii <- k.t >= 0 & k.t < 0.22
  k.d[ii] <- 1 - 0.09 * k.t[ii]
  ii <- k.t <= 0.8 & k.t >= 0.22
  k.d[ii] <- 0.9511 - 0.1604 * k.t[ii] + 4.388 * k.t[ii]^2 - 
    16.638 * k.t[ii]^3 + 12.336 * k.t[ii]^4
  ii <- k.t > 0.8
  k.d[ii] <-  0.165
  # Direct Normal Irradiance
  # DNI <- x[[GHI]] * (1 - k.d) / cospi(x$zenith_avr / 180)
  DNI <- rep(0, nrow(x)); DHI <- DNI
  DNI[zz] <- x[[GHI]][zz] * (1 - k.d[zz]) / cospi(x[[zenith]][zz] / 180)
  # Diffuse Horizontal Irradiance
  DHI <- x[[GHI]] * k.d 
  if (keep.all) {
    x$ext_irrad <- Ge
    x$clearness_index <- k.t
    x$diffuse_fraction <- k.d
  }
  x$DNI <- DNI
  x$DHI <- DHI
  return(x)
}


diffuse_fraction <- function(yday, zenith, GHI) {
  # the solar constant
  Gsc <- 1360.8
  # Extraterrestrial irradiance
  Ge <- (1 + 0.033 * cos(360 * yday / 365)) * Gsc
  # Clearness index
  k.t <- GHI / Ge / cospi(zenith / 180)
  # Diffuse fraction
  k.d <- rep(0, nrow(x))
  ii <- k.t > 0 & k.t < 0.22
  k.d[ii] <- 1 - 0.09 * k.t[ii]
  ii <- k.t <= 0.8 & k.t >= 0.22
  k.d[ii] <- 0.9511 - 0.1604 * k.t[ii] + 4.388 * k.t[ii]^2 - 
    16.638 * k.t[ii]^3 + 12.336 * k.t[ii]^4
  ii <- k.t > 0.8
  k.d[ii] <-  0.165
  return(k.d)
}

if (F) {
  # system.time(z1 <- solar_irradiance(y))
  # system.time(z2 <- diffuse_fraction(y$yday, y$zenith, y$GHI))
  # identical(z1, z2)
  
  z <- solar_irradiance(y, keep.all = T, zenith_max = 85)
  summary(z$DNI)
  summary(z$DHI)
  summary(z$clearness_index)
  summary(z$diffuse_fraction)
}
