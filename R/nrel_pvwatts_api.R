hour_utc2tz <- function(hour_utc, lon = NULL, lat = NULL, tz_offset = NULL) {
  if (is.null(tz_offset)) tz_offset <- coord2tz_offset(lon, lat)
  hour_tz <- (hour_utc + tz_offset + 48) %% 24
  return(as.integer(hour_tz))
}

coord2tz_offset <- function(lon, lat, utc = NULL) {
  .tz <- lutz::tz_lookup_coords(lat = lat, lon = lon, 
                               method = "fast", warn = FALSE)
  # browser()
  .off <- rep(NA, length(lon))
  if (is.null(utc)) utc <- rep(lubridate::ymd_h("2010-01-01 00"), length(lon))
  for (i in unique(.tz)) {
    ii <- .tz == i
    suppressWarnings({
      .off[ii] <- lutz::tz_offset(utc[ii], i)$utc_offset_h
    })
  }
  return(.off)
}

if (F) {
  lutz::tz_lookup_coords(lat = merra$lat[1:2], lon = merra$lon[1:2], 
                         method = "fast", warn = FALSE)
  coord2tz_offset(lon = merra$lon[1:2], 
                  lat = merra$lat[1:2])
  coord2tz_offset(lon = merra$lon[1:2], 
                  lat = merra$lat[1:2],
                  merra$datetime[1:2])
  hour_utc2tz(0,0,0)
  hour_utc2tz(0,180,0)
  hour_utc2tz(0,-180,0)
  hour_utc2tz(0,-10,0)
  
}

yday2month <- function(yday, leap_year = FALSE) {
  if (leap_year) x <- "2012-01-01" else x <- "2010-01-01"
  x <- lubridate::ymd(x) + lubridate::days(yday - 1)
  return(lubridate::month(x))
}


#' Title
#'
#' @param query 
#' @param ... 
#' @param asList 
#' @param asJSON 
#' @param pvwatts_api_url 
#' @param api_key 
#' @param array.type 
#'
#' @details <https://developer.nrel.gov/docs/solar/pvwatts/v6/>
#'
#' @return
#' @export
#'
#' @examples
fetch_pvwatts <- function(
  query = list(
    lat = 0, lon = 0, dataset = "intl", radius = 1000, 
    system_capacity = 1, module_type = 1, losses = 10,
    array_type = 0, tilt = 0, azimuth = 0, timeframe = "hourly"),
  array.type = NULL,
  ..., asList = TRUE, asJSON = !asList,
  pvwatts_api_url = "https://developer.nrel.gov/api/pvwatts/v6.json?api_key=",
  api_key = getOption("pwatts.api.key")) {
  
  args <- list(...)
  for (a in names(args)) {
    query[a] <- args[a]
  }
  
  if (!is.null(array.type)) {
    stopifnot(length(array.type) == 1)
    if (array.type == "fh") {
      query$array_type <- 0
      query$tilt <- 0
    } else if (array.type == "fl") {
      query$array_type <- 0
      query$tilt <- abs(query$lat)
    } else if (array.type == "th") {
      query$array_type <- 2
      query$tilt <- 0
    } else if (array.type == "tl") {
      query$array_type <- 2
      query$tilt <- abs(query$lat)
    } else if (array.type == "tv") {
      warning("The array type is not available")
      return(list())
    } else if (array.type == "td") {
      query$array_type <- 4
      query$tilt <- 0 # modeled by PVWatts
    } else {
      
    }   
  }
  # browser()
  pvwatts_url <- paste0(pvwatts_api_url, api_key)
  x <- httr::GET(url = pvwatts_url, query = query)
  if (asJSON) return(x)
  x <- jsonlite::fromJSON(httr::content(x, "text"), simplifyVector = FALSE)
  return(x)
}


if (F) {
  a <- fetch_pvwatts(lat = 80, lon = -80, radius = 1000, tilt = 80,
                   azimuth = 180)
  a$station_info
  summary(unlist(a$outputs$poa))
  
  class(a)
  names(a)
  class(a$inputs)
  names(a$inputs)
  a$inputs
  
  a$errors
  
  
}

#' Title
#'
#' @param lon 
#' @param lat 
#' @param asList 
#'
#' @return
#' @export
#'
#' @examples
closest_locid <- function(lon, lat, asList = FALSE) {
  # browser()
  stopifnot(length(lon) == length(lat))
  x <- data.frame(lon = lon, lat = lat)
  # l_id <- locid[,1:2]
  if (asList) id <- list() else id <- numeric()
  for (i in 1:length(lon)) {
    d <- geosphere::distm(x[i,], locid[,1:2])
    if (asList) {
      id[[i]] <- which(d[1,] == min(d[1,]))
    } else {
      id[i] <- which(d[1,] == min(d[1,]))[1]
    }
  }
  return(id)
}

