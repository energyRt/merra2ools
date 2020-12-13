
#' Irradiance and its components
#'
#' @return
#' \loadmathjax
#' List or data.frame with estimated following solar geometry variables:
#' \itemize{
#'   \item Extraterrestrial irradiance (\mjseqn{G_e})
#'     \mjsdeqn{G_e = G_{sc}\times\big(1+0.033\cos{(\frac{360n}{365})}\big)}
#'     where: \cr
#'     \mjseqn{G_{sc} = 1360.8W/m^2}, is the solar constant based on the latest 
#'     NASA observation (Kopp \& Lean, 2011); \cr
#'     \mjseqn{n - \text{day of the year}} \cr
#'     
#'   \item Clearness index (\mjseqn{G_e})
#'     \mjsdeqn{k_t = \frac{GHI}{G_e\cos{(zenith)}}}
#'     
#'   \item Diffuse fraction (\mjseqn{ k_d})
#'   
#'   \item DHI and DNI
#' } 
#'     
#' @export
#'
# @examples
solar_irradiance <- function() {
  
}