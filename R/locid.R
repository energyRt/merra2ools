#' Get MERRA-2 grid
#'
#' @param type type of grid-data to return, spatial points ("points") or polygons ("poly")
#' @param lon numeric vector (min and max) with the range of longitude coordinates of the grid to return. Default `c(-180, 180)`.
#' @param lat numeric vector with the range of latitude coordinates of the grid to return. Default `c(-180, 180)`.
#' @param locid (optional) integer vector of location identifiers for which the grid will be returned.
#' @param crs target coordinate reference system: object of class 'sf::crs', or input string for `sf::st_crs`. Default `4326`.
#' @param ... 
#' @param add_lonlat logical, should merra-points coordinates (`lon`, `lat`) be added to the data. FALSE by default.
#' @param add_poles_points logical, in the case of "polygons" grid, should points at poles be added to the data. TRUE by default.
#'
#' @return Returns `sf` object with MERRA-2 grid, points or polygons. If polygons requested, grid of  will be returned where MERRA2 coordinates are considered as centers of every polygon, except cells with `lat = -90` or `lat = 90`. Spatial points will be returned for the cells near poles.
#' @export
#'
#' @examples
#' x <- get_merra_grid()
#' head(x)
#' getGrid("poly", "sf")
#' getGrid(lon = c(-70, -60), lat = c(30, 40), class = "df")
get_merra_grid <- function(type = "polygons", 
                           locid = NULL,
                           lon = c(-180, 180), 
                           lat = c(-90, 90),
                           crs = 4326,
                           add_lonlat = FALSE,
                           add_poles_points = TRUE,
                           ...) {
  if (grepl("poly", type)[1]) {
    x <- merra2ools:::locid_poly_sf %>% as.data.table()
  } else if (grepl("point", type)[1]) {
    x <- merra2ools:::locid_points_sf %>% as.data.table()
  } else {
    stop("Unknown type ", type)
  }
  x <- st_as_sf(x)
  
  if (!is.null(locid)) {x <- x[locid,]}
  if (!add_poles_points & type == "polygons") {
    # cc <- sapply(x$geometry, class)
    # dim(cc)
    # summary(as.factor(cc[2,]))
    # ii <- cc[2,] == "POINT"; summary(ii)
    # which(ii[1:1e3]) %>% range()
    # which(ii) %>% range()
    # ii <- c(1:576, 207361:207936)
    ii <- 577:207360
    x <- x[ii,]; rm(ii)
  }
  
  if (!is.null(lon) && any(lon != c(-180, 180))) {
    ii <- x$lon >= lon[1] & x$lon <= lon[2]
    x <- x[ii,]
  }
  if (!is.null(lat) && any(lat != c(-90, 90))) {
    ii <- x$lat >= lat[1] & x$lat <= lat[2]
    x <- x[ii,]
  }
  if (!add_lonlat) {x$lon <- NULL; x$lat <- NULL}
  # if (!add_lonlat) x[, c("lon", "lat") := NULL]
  x <- st_as_sf(x)
  if (sf::st_crs(x) != sf::st_crs(crs)) x <- sf::st_transform(x, crs)
  return(x)
}

if (F) {
  x <- get_merra_grid()
  x <- get_merra_grid(add_poles_points = F)
  x; class(x)
  x <- get_merra_grid(lon = c(-170, 170))
  x <- get_merra_grid(lon = c(-20, 10), lat = c(-10, 10), add_lonlat = F)
  x <- get_merra_grid(locid = sample(locid$locid, 100))
  st_bbox(x); class(x)
  plot(x)
}

#' Get MERRA-2 grid IDs which overlaps with the given spatial object
#'
#' @param x spatial (`sf`) object.
#' @param method 
#' @param ... ignored
#'
#' @return
#' @export
#'
#' @examples
get_locid <- function(x, method = "polygons", add_poles_points = F, 
                      return_grid = FALSE, ...) {
  # browser()
  x <- sf::st_make_valid(x) %>% sf::st_union()
  if (grepl("poly", method, ignore.case = T)) {
    
  } else if (grepl("point", method, ignore.case = T)) {
    
  } else {
    stop("Unknown method: ", method)
  }
  
  g <- get_merra_grid(type = method, add_poles_points = add_poles_points)
  
  if (sf::st_crs(x) != sf::st_crs(g)) g <- sf::st_set_crs(g, sf::st_crs(x))
  suppressWarnings({
    # complains about bbox - checked
    a <- sf::st_intersects(g, x)
  })
  nn <- sapply(a, is_empty)
  if (return_grid) return(g[!nn,])
  return(g$locid[!nn])
}
    
if (F) {
  y <- get_locid(gis_sf, return_grid = T)
  length(y); head(y)
  
  yp <- get_locid(gis_sf, method = "points", return_grid = T)
  length(y); head(y)
  
  plot(gis_sf$geometry, reset = F, col = "wheat")
  plot(y, add = T, col = NA, border = "blue")
  plot(yp, add = T, col = "red", pch = 16, cex = .25)
  
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
