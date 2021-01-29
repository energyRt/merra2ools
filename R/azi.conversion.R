##coverting "absolute" azimuth to "relative" azimuth.
###"absolute": North is 0, East is 90, South is 180, West is 270. Independent of Northern or Southern hemisphere.(The azimuth results of solar_position() is in this coordinates)
###"relative": The deriction facing the equator is 0, West is 90, East is -90. (Inputs of pv_array_positon() needs to be in this format)

azimuth.convertion <- function(x){
  ii1 <- x$lat>=0
  ii2 <- x$lat<0&x$azimuth<180
  ii3 <- x$lat<0&x$azimuth>=180
  x$azimuth[ii1] <- x$azimuth[ii1]-180
  x$azimuth[ii2] <- -x$azimuth[ii2]
  x$azimuth[ii3] <- 360-x$azimuth[ii3]
  return(x)
}
