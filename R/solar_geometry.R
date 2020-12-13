# Solar geometry and radiation

yday365 <- function(yday) {
  # leap year normalization
  yday[yday == 366L] <- 365L
  stopifnot(all(yday >= 1L))
  stopifnot(all(yday <= 365L))
  return(yday)
}


solar_declination <- function(yday, check_yday = TRUE) {
  # theta.d - solar declination in radians
  if (check_yday) yday <- yday365(yday)
  return(23.45 * pi / 180 * sinpi(2 * (284 + yday) / 365))
}


#' Solar geometry
#'
#' @param datetime date and time
#' @param yday day of a year
#' @param tz timezone
#'
#' @return 
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
#' @export
#' @import mathjaxr
#'
#' @examples
#' NA
solar_geometry <- function(datetime = NULL, yday = NULL, tz = lubridate::tz(datetime)) {
  
  if (is.null(datetime)) {
    if (is.null(yday)) {stop("Either 'datetime' or 'yday' should be provided")}
  } else {
    if (!is.null(yday)) {stop("Only one of 'datetime' or 'yday' should be provided")}
    
    yday <- lubridate::yday(datetime)
  }
  yday <- yday365(yday)
  
  # Calculate solar declination
  declination <- solar_declination(yday = yday, check_yday = FALSE)
  
  return(list(
    declination = declination
  ))

}

if (F) {
  mathjaxr::preview_rd("solar_geometry", type = "pdf")
  
}