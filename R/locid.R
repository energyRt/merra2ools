#' Get MERRA-2 grid
#'
#' @param type type of grid-data to return, spatial points ("points") or polygons ("poly")
#' @param lon range of longitude
#' @param lat range of latitude
#' @param class class of object to return, spatial ("sp"), simple feature ("sf"), or data.frame ("df")
#'
#' @return Returns object with MERRA-2 grid information
#' @export
#'
#' @examples
#' x <- getGrid()
#' head(x)
#' getGrid("poly", "sf")
#' getGrid(lon = c(-70, -60), lat = c(30, 40), class = "df")
getGrid <- function(type = "points", class = "sp", 
                    locid = NULL,
                    lon = c(-180, 180), lat = c(-90, 90)) {
  if (grepl("points", type, ignore.case = T)) {
    x <- merra2ools::locid[, c("locid", "lon", "lat")]
    if (!grepl("df|data.frame", class, ignore.case = T)) {
      x$lon2 <- x$lon; x$lat2 <- x$lat
      sp::coordinates(x) <- ~ lon2 + lat2
      sp::proj4string(x) <- grid@proj4string
    }
  } else if (grepl("poly", type, ignore.case = T)) {
    x <- merra2ools::grid
  } else {
    stop("Unknown type = '", type, "'")
  }
  # filter
  ii <- x$lon >= lon[1] & x$lon <= lon[2] 
  ii <- ii & x$lat >= lat[1] & x$lat <= lat[2]
  if (!is.null(locid)) ii <- ii & x$locid %in% locid
  x <- x[ii,]
  if (class == "sf") {
    cat("Converting sp object to simple feature (sf) format")
    x <- sf::st_as_sf(x) 
  } else if (grepl("df|data.frame", class, ignore.case = T)) {
    cat("Converting sp object to data.frame format")
    if (grepl("poly", type, ignore.case = T)) {
      x <- ggplot2::fortify(x)
    }
  } else if (class != "sp") {
    stop("Unknown data format requested: class = '", class, "'")
  }
  return(x)
}

if (F) {
  x <- getGrid()
  head(x)
  getGrid("poly", "sf")
  getGrid(class = "sf")
  getGrid(lon = c(-70, -60), lat = c(30, 40), class = "df")
  getGrid("poly", lon = c(-70, -60), lat = c(30, 40), class = "df")
  getGrid(lon = c(-70, -60), lat = c(30, 40), class = "sf")
  getGrid("poly", lon = c(-70, -60), lat = c(30, 40), class = "sf")
}


#' Get MERRA-2 grid IDs that overlay or intersect with given spatial object
#'
#' @param sp a map-object of class SpatialPolygons or SpatialPolygonsDataFrame
#' @param method character, "points" or "intersect", matching spatial points with polygons or intersection of polygons respectively
#' @param return_sp logical, if TRUE, Spatial object will be returned
#' @param projString projection string for return object (in the case of return_sp = TRUE)
#'
#' @return when return_sp = FALSE, an integer vector with location IDs (locid) is returned
#' @export
#'
#' @examples
#' iceland_sp <- rnaturalearth::ne_states(country = "iceland")
#' lid2_sp <- get_locid(iceland_sp, method = "intersect", return_sp = TRUE)
#' sp::plot(lid2_sp, col = "wheat", axes = TRUE, main = "Iceland")
#' lid1 <- get_locid(iceland_sp, method = "points")
#' lid2 <- get_locid(iceland_sp, method = "intersect", )
#' lid1_df <- getGrid(locid = lid1, class = "df")
#' lid2_df <- getGrid(locid = lid2, class = "df")
#' points(lid2_df$lon, lid2_df$lat, col = "red")
#' points(lid1_df$lon, lid1_df$lat, col = "red", pch = 16)
#' 
get_locid <- function(sp, method = "intersect", return_sp = FALSE, projString = NULL) {
  if (class(sp) == "SpatialPolygonsDataFrame") {
    sp <- sp::SpatialPolygons(sp@polygons, proj4string = sp@proj4string)
  } else if (class(sp) != "SpatialPolygons") {
    stop("'sp' object should be 'SpatialPolygons' or 'SpatialPolygonsDataFrame'")
  }
  if (is.null(projString)) projString <- grid@proj4string
  # sp::proj4string(sp) <- projString
  raster::crs(sp) <- projString
  
  lon_range <- c(floor(sp@bbox[1,1]), ceiling(sp@bbox[1,2]))
  lat_range <- c(floor(sp@bbox[2,1]), ceiling(sp@bbox[2,2]))
  
  if (method == "points") {
    grid_sp_points <- getGrid(lon = lon_range, lat = lat_range)
    if (!sp::identicalCRS(sp, grid_sp_points)) {
      # sp::proj4string(grid_sp_points) <- projString
      raster::crs(grid_sp_points) <- projString
    }
    pp <- sp::over(grid_sp_points, sp, returnList = F)
    if (!return_sp) {
      pp <- pp[!is.na(pp)]
      pp <- as.integer(names(pp))
    }
    return(pp)
    
  } else if (method == "intersect") {
    grid_sp_poly <- getGrid(lon = lon_range, lat = lat_range, type = "poly")
    if (!sp::identicalCRS(sp, grid_sp_poly)) {
      # sp::proj4string(grid_sp_points) <- projString
      raster::crs(grid_sp_poly) <- projString
    }
    # ri <- rgeos::gIntersection(sp, grid_sp_poly, byid = TRUE, )
    ri <- raster::intersect(grid_sp_poly, sp)
    if (return_sp) return(ri)
    ids <- unique(ri$locid)
    return(ids)
  }
  
}

if (F) {
  iceland_sp <- rnaturalearth::ne_states(country = "iceland")
  lid2_sp <- get_locid(iceland_sp, method = "intersect", return_sp = TRUE)
  sp::plot(lid2_sp, col = "wheat", axes = TRUE, main = "Iceland")
  lid1 <- get_locid(iceland_sp, method = "points")
  lid2 <- get_locid(iceland_sp, method = "intersect", )
  lid1_df <- getGrid(locid = lid1, class = "df")
  lid2_df <- getGrid(locid = lid2, class = "df")
  points(lid2_df$lon, lid2_df$lat, col = "red")
  points(lid1_df$lon, lid1_df$lat, col = "red", pch = 16)
  
}


#' Get MERRA-2 grid IDs closest to the given coordinates
#'
#' @param lon longitude in degrees (-180 <= lon <= 180)
#' @param lat latitude in degrees (-90 <= lat <= 90)
#' @param asList 
#'
#' @return 
#' integer vector with locations IDs (locid) when `asList` is FALSE (default). In the case of several values, only the first `locid` will be returned. If `asList` is TRUE, a list is returned with possible multiple values for each coordinate.
#' 
#' @export
#'
#' @examples
#' closest_locid(0, 0)
#' closest_locid(100.14, -85.145)
#' closest_locid(0, 89.5, asList = TRUE)
#' 
closest_locid <- function(lon, lat, asList = FALSE) {
  # browser()
  nlon <- length(lon); nlat <- length(lat)
  stopifnot(nlon >= 1); stopifnot(nlat >= 1)
  if (nlon != nlat) {
    if (nlon == 1 & nlat > 1) {
      lon <- rep(lon, nlat)
    } else if (nlon > 1 & nlat == 1) {
      lat <- rep(lat, nlon)
    } else {
      stop("Inconsistent lenghs of `lon` and `lat`")
    }
  }
  x <- data.frame(lon = lon, lat = lat)
  # l_id <- locid[,1:2]
  if (asList) id <- list() else id <- integer()
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
