## converting "absolute" azimuth to "relative" azimuth.
### "absolute": North is 0, East is 90, South is 180, West is 270. Independent of Northern or Southern hemisphere.(The azimuth results of solar_position() is in this coordinates)
### "relative": The direction facing the equator is 0, West is 90, East is -90. (Inputs of pv_array_position() needs to be in this format)

azimuth.convertion <- function(x) {
  ii1 <- x$lat >= 0
  ii2 <- x$lat < 0 & x$azimuth < 180
  ii3 <- x$lat < 0 & x$azimuth >= 180
  x$azimuth[ii1] <- x$azimuth[ii1] - 180
  x$azimuth[ii2] <- -x$azimuth[ii2]
  x$azimuth[ii3] <- 360 - x$azimuth[ii3]
  return(x)
}


#' Title
#'
#' @param x 
#' @param azimuth_N 
#' @param azimuth_Q 
#' @param lat 
#' @param lon 
#'
#' @return
#' @export
#'
#' @examples
azimuth_N2Q <- function(x, 
                        azimuth_N = "azimuth_N", 
                        azimuth_Q = "azimuth_Q",
                        lat = "lat", lon = "lon") {
  # browser()
  stopifnot(length(azimuth_N) == length(azimuth_Q))
  stopifnot(!is.null(x[[lat]]))
  for (i in 1:length(azimuth_N)) {
    stopifnot(!is.null(x[[azimuth_N[i]]]))
    x[[azimuth_Q[i]]] <- -x[[azimuth_N[i]]] # valid for x$lat < 0 & x$azimuth < 180
    x[[azimuth_Q[i]]][is.na(x[[lat]])] <- NA # avoiding potential errors when lat is NA
    ii <- x[[lat]] >= 0
    x[[azimuth_Q[i]]][ii] <- x[[azimuth_N[i]]][ii] - 180
    ii <- !ii & x[[azimuth_N[i]]] >= 180 
    x[[azimuth_Q[i]]][ii] <- 360 - x[[azimuth_N[i]]][ii]
    x[[azimuth_Q[i]]][x[[azimuth_Q[i]]] == -180] <- 180
  }
  return(x)
}


azimuth_Q2N <- function(x, 
                        azimuth_Q = "azimuth_Q", 
                        azimuth_N = "azimuth_N", 
                        lat = "lat", lon = "lon") {
  stopifnot(length(azimuth_N) == length(azimuth_Q))
  stopifnot(!is.null(x[[lat]]))
  for (i in 1:length(azimuth_N)) {
    stopifnot(!is.null(x[[azimuth_Q[i]]]))
    x[[azimuth_N[i]]] <- x[[azimuth_Q[i]]] + 180 # for Northern hemisphere
    x[[azimuth_N[i]]][is.na(x[[lat]])] <- NA # avoiding potential errors when lat is NA
    ii <- x[[lat]] < 0
    x[[azimuth_N[i]]][ii] <- -x[[azimuth_Q[i]]][ii]
    ii <- ii & x[[azimuth_Q[i]]] > 0
    x[[azimuth_N[i]]][ii] <- 360 - x[[azimuth_Q[i]]][ii]
    x[[azimuth_N[i]]][x[[azimuth_N[i]]] == 360] <- 0
  }
  return(x)
}


if (F) {
  x <- expand.grid(azimuth = seq(0, 360, by = 5),
                   lat = seq(-90, 90, by = 5))
  x <- azimuth_N2Q(x) #%>% as_tibble()
  x <- azimuth_Q2N(x)
  ii <- x$lat < 0 
  plot(x$azimuth[ii], x$azimuth_Q[ii], col = "blue", pch = 16, cex = 2, 
       ylim = c(-180, 180), xlim = c(0, 360))
  abline(h = c(-180, 0, 180), col = "grey")
  abline(v = c(0, 360), col = "grey")
  points(x$azimuth[!ii], x$azimuth_Q[!ii], col = "red", pch = 16, cex = 1.5)
  
  plot(x$azimuth, x$azimuth_N, col = "brown", pch = 16, cex = 1.5)
  summary(x$azimuth); summary(x$azimuth_Q); summary(x$azimuth_N)
}