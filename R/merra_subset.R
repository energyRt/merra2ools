#' Create date-time character string from year, month, hour, and timezone.
#'
#' @details
#' A utility function to create input in format used `get_merra2_subset`.
#' 
#' @param year integer, year
#' @param month integer, month (1-12)
#' @param day integer (1-31) or character ("last"), day of the month.
#' @param hour integer, hour (0-23)
#' @param tz Olson timezone code (`OlsonNames()`), "UTC" by default.
#'
#' @return
#' @export
#'
#' @examples
#' fDate(2010, 12, "last", 23)
#' fDate(2010, 1, 1, 0)
#' OlsonNames()
#' fDate(2010, 1, 1, 0, tz = "America/New_York")
#' fDate(2010, 1, 1, 0, tz = "Pacific/Auckland")
#' fDate(2010, 1, 1, 0, tz = "Asia/Kolkata")
fDate <- function(year, month, day, hour, tz = "UTC") {
  # browser()
  if (is.numeric(year)) year <- as.integer(year)
  if (is.numeric(month)) month <- as.integer(month)
  if (is.numeric(day)) day <- as.integer(day)
  if (is.numeric(hour)) hour <- as.integer(hour)
  stopifnot(hour >= 0 & hour <= 23)
  stopifnot(month >= 1 & month <= 12)
  f <- function(x) formatC(x, width = 2, flag = "0")
  if (day == "last" || is.numeric(day) > 31) {
    day <- lubridate::days_in_month(ymd(paste(year, f(month), f(1), sep = "-")))
  }
  x <- paste(paste(year, f(month), f(day), sep = "-"), f(hour))
  if (tz != "UTC") {
    x <- lubridate::ymd_h(x, tz = tz)
    x <- lubridate::with_tz(x, tzone = "UTC")
    x <- format(x, format = "%F %H")
  }
  return(x)
}

if (F) {
  fDate(2010, 12, "last", 23)
  # OlsonNames()
  fDate(2010, 1, 1, 0)
  fDate(2010, 1, 1, 0, tz = "America/New_York")
  fDate(2010, 1, 1, 0, tz = "Pacific/Auckland")
  fDate(2010, 1, 1, 0, tz = "Asia/Kolkata")
  lubridate::with_tz(lubridate::ymd_h("2010-01-01 00", tz = "Asia/Kolkata"), 
                     tzone = "UTC")
}

#' Get MERRA-2 subset 
#'
#' @param from starting date and time of the subset in "YYYY-MM-DD HH" format
#' @param to ending date and time of the subset in "YYYY-MM-DD HH" format (see \code{fDate})
#' @param tz time zone of the requested date-time interval (see \code{OlsonNames})
#' @param locid integer vector with location \code{locid} (values from 1 to 207936)
#' @param cols names or indexes of columns from the database (NULL default, returning all available columns)
#' @param quiet if process should be reported
#' @param rows_lim 
#' @param original.units 
#'
#' @return
#' data.table with the subset of 
#' @export
#' @references 
#' <https://en.wikipedia.org/wiki/List_of_tz_database_time_zones>
#' @examples
#' NA
get_merra2_subset <- function(locid = 1:207936L, 
                              from = "1980-01-01 00", to = "2020-12-31 23", 
                              tz = "UTC", 
                              cols = NULL,
                              quiet = FALSE,
                              rows_lim = 2*10^9,
                              as_integers = FALSE) {
  # browser()
  # if (!any(grepl("package:fst", search()))) library("fst")
  
  raw_cols <- c("UTC",  "locid", "W10M.e1", "W50M.e1", "WDIR.e_1", "T10M.C", 
                "SWGDN", "ALBEDO.e2", "PRECTOTCORR.g_m2_h.e1", "RHOA.e2")
  
  if (!is.null(cols)) {
    if (is.numeric(cols)) {
      cols <- raw_cols[cols]
    }
  }

  from <- lubridate::ymd_h(from, tz = tz) 
  from <- lubridate::with_tz(from, tzone = "UTC") 
  if (minute(from) != 0L) {
    warning(
      "Inconsistent time averages due to fractual difference between the requested timezone and UTC"
    )
  }
  from <- lubridate::floor_date(from, unit = "hour") + lubridate::minutes(30L)

  to <- lubridate::ymd_h(to, tz = tz) #+ lubridate::minutes(30L)
  to <- lubridate::with_tz(to, tzone = "UTC")
  to <- lubridate::floor_date(to, unit = "hour") + lubridate::minutes(30L)

  stopifnot(to >= from)
  
  from_to_h <- seq(from, to, by = "hour")
  
  yrs <- seq(lubridate::year(from), lubridate::year(to))
  yyyymm <- unique(format(seq(as.Date(from), as.Date(to), by = "day"), 
                          format = "%Y%m"))
  mr <- check_merra2(detailed = T)
  
  fls <- mr$merra_files[grepl(paste(yyyymm, collapse = "|"), mr$merra_files)]
  nrows_est <- 24 * 30 * length(locid) * length(fls)
  if (nrows_est > rows_lim) {
    message("The request might exeed rows' number limit ('rows_lim').\nTry to reduce the number of locations ('locid') and/or the time interval ('from' & 'to')")
    return(invisible(NULL))
  }
  # browser()
  sam <- NULL
  # for (f in 1:length(fls)) {
  for (i in 1:length(yyyymm)) {
    if (!quiet) cat("file:", fls[i])
    # if (!quiet) cat("file:", fls[f])
    # sam_i <- fst::read_fst(file.path(mr$path, fls[f]), as.data.table = T)
    sam_i <- read_merra_file(yyyymm[i], as_integers = TRUE)
    # if (!is.null(sam_i[["loc_id"]])) sam_i <- dplyr::rename(sam_i, locid = loc_id)
    # if (!is.null(sam_i[["datetime"]])) sam_i <- dplyr::rename(sam_i, UTC = datetime)
    if (!is.null(cols)) {
      raw_cols <- names(sam_i)
      raw_cols <- raw_cols[grepl(paste(cols, collapse = "|"), raw_cols)]
      raw_cols <- unique(c("UTC", "locid", raw_cols))
      sam_i <- sam_i[, ..raw_cols]    
    }
    # ii <- sam_i$locid %in% locid
    # sam_i <- sam_i[ii,]
    # ii <- sam_i$UTC %in% from_to_h
    # sam_i <- sam_i[ii,]
    LOCID <- locid
    sam_i <- sam_i[locid %in% LOCID][UTC %in% from_to_h]
    # browser()

    if (is.null(sam)) {
      sam <- sam_i
    } else {
      # sam <- dplyr::bind_rows(sam, sam_i)
      sam <- rbind(sam, sam_i)
    }
    if (!quiet) {
      cat(", ", format(object.size(sam), units = "auto"), 
          ", nrow = ", nrow(sam), 
          ", ncol = ", ncol(sam), 
          "\n", sep = "")
    }
  }
  # options(dplyr.summarise.inform = op)
  # browser()
  if (!as_integers) {
    # if (!is.null(sam[["W10M"]])) sam[["W10M"]] <- sam[["W10M"]]/10
    # if (!is.null(sam[["W50M"]])) sam[["W50M"]] <- sam[["W50M"]]/10
    # if (!is.null(sam[["ALBEDO"]])) sam[["ALBEDO"]] <- sam[["ALBEDO"]]/100
    sam <- merra2ools:::convert_units(sam)
  }
  return(sam)
}

if (F) {
  m <- get_merra2_subset(1e5, fDate(2010, 1, 1, 00), to = fDate(2010, 1, 2, 23))
  m <- get_merra2_subset(1e5, fDate(2010, 1, 1, 00), to = fDate(2010, 1, 2, 23), 
                         tz = "Asia/Kolkata")
}



#' Reads one file from MERRA-2 subset for a given year and month
#'
#' @param YYYYMM character, year and month
#' @param path the database path
#' @param original.units should the original MERRA-2 units be returned (converted from integer vectors)
#'
#' @return
#' A data.table with the MERRA-2 subset for the given year and month.
#' @export
#'
#' @examples
#' \dontrun{
#' read_merra_file("201912")
#' }
read_merra_file <- function(YYYYMM, 
                            path = get_merra2_dir(), 
                            as_integers = FALSE) {
  # Internal function
  if (!any(grepl("package:fst", search()))) library("fst")
  
  YYYYMM <- gsub("-", "", YYYYMM)
  YYYYMM <- gsub("/", "", YYYYMM)
  YYYYMM <- gsub("_", "", YYYYMM)
  YYYYMM <- gsub(" ", "", YYYYMM)
  
  if (!check_merra2(path, verbose = F)) {
    stop("MERRA-2 subset is not found in path = ", path)
  }
  fl <- file.path(path, paste0("merra2_", YYYYMM, ".fst"))
  if (!file.exists(fl)) {
    warning("File ", fl, " does not exist")
    return(NULL)
  }
  merra <- fst::read_fst(fl, as.data.table = T)
  # browser()
  if (!is.null(merra[["loc_id"]])) merra <- dplyr::rename(merra, locid = loc_id)
  if (!is.null(merra[["datetime"]])) merra <- dplyr::rename(merra, UTC = datetime)
  
  if (!as_integers) {merra <- convert_units(merra)}
  
  return(merra)
}

convert_units <- function(merra) {
    if (!is.null(merra[["W10M.e1"]])) {
      merra[["W10M.e1"]] <- merra[["W10M.e1"]]/10
      merra <- dplyr::rename(merra, W10M = W10M.e1)
    }
    if (!is.null(merra[["W50M.e1"]])) {
      merra[["W50M.e1"]] <- merra[["W50M.e1"]]/10
      merra <- dplyr::rename(merra, W50M = W50M.e1)
    }
    if (!is.null(merra[["WDIR.e_1"]])) {
      merra[["WDIR.e_1"]] <- merra[["WDIR.e_1"]]*10L
      merra <- dplyr::rename(merra, WDIR = WDIR.e_1)
    }
    if (!is.null(merra[["T10M.C"]])) {
      merra <- dplyr::rename(merra, T10M = T10M.C)
    }
    if (!is.null(merra[["ALBEDO.e2"]])) {
      merra[["ALBEDO.e2"]] <- merra[["ALBEDO.e2"]]/100L
      merra <- dplyr::rename(merra, ALBEDO = ALBEDO.e2)
    }
    if (!is.null(merra[["PRECTOTCORR.kg_m2_h.e1"]])) {
      merra[["PRECTOTCORR.kg_m2_h.e1"]] <- merra[["PRECTOTCORR.kg_m2_h.e1"]]/10L # convert to kg
      merra <- dplyr::rename(merra, PRECTOTCORR = PRECTOTCORR.kg_m2_h.e1)
    }
    if (!is.null(merra[["RHOA.e2"]])) {
      merra[["RHOA.e2"]] <- merra[["RHOA.e2"]]/100L
      merra <- dplyr::rename(merra, RHOA = RHOA.e2)
    }
    
    return(merra)
  # }
  
}


if (F) {
  read_merra_file("200001", as_integers = T)
  read_merra_file("200001", as_integers = F)
  read_merra_file("200001", as_integers = T) %>% summary()
  read_merra_file("198001", as_integers = T)
  read_merra_file("202012", as_integers = T)
  read_merra_file("202012", as_integers = F)
  
}

#' Add longitude and latitude of 'locid'
#'
#' @param x data.frame with `locid` column.
#' @param replace logical, if TRUE, existing `lon` and `lat` columns in `x` will be replaced.
#'
#' @return `x` with added/replaced `lon` and `lat` for each `locid`
#' @export
#'
#' @examples
#' NA
add_coord <- function(x, replace = FALSE) {
  # browser()
  if (is.null(x[["locid"]])) stop("'x' should have 'locid' column")
  if (!is.null(x[["lon"]]) || !is.null(x[["lat"]])) {
    if (replace) {
      x$lon <- NULL
      x$lat <- NULL
    } else {
      stop("'x' already has 'lon' and/or 'lat' coordinates, use 'replace = TRUE' to overwrite")
    }
  }
  # x <- dplyr::left_join(x, locid[,1:3], by = "locid")
  # y <- data.table::as.data.table(locid[,1:3])
  
  x <- merge(x, locid[,1:3], by = "locid", 
             all.x = TRUE, all.y = FALSE, sort = FALSE)
  return(x)
}

if (F) {
  add_coord(merra2_apr)
}


#' Adds MERRA2 grid (polygons) to the data
#'
#' @param x data.frame with `locid` column
#'
#' @return `x` as `sf` object, with added geometry (polygons)
#' @export
#'
#' @examples
add_merra2_grid <- function(x) {
  merra2ools:::locid_poly_sf[, c("locid", "geometry")] %>%
    dplyr::right_join(x, by = "locid")
}

#' Adds MERRA2 grid (points) to the data
#'
#' @param x data.frame with `locid` column
#'
#' @return `x` as `sf` object, with added geometry (points)
#' @export
#'
#' @examples
add_merra2_points <- function(x) {
  merra2ools:::locid_points_sf[, c("locid", "geometry")] %>%
    dplyr::right_join(x, by = "locid")
}



add_locid <- function(x) {
  
}

#' Get a example of MERRA2 data for a particular month(s)
#'
#' @param month integer vector with month numbers (1 to 12)
#'
#' @return
#' An example of MERRA-2 data set for a requested month, 21st day, 2010. 
#' The returned data.table has the same format as returned by \code{get_merra2_subset()}.
#' 
#' @export
#'
#' @examples
#'  merra2_sample()
#'  merra2_sample(2:3)
merra2_sample <- function(month = 1:12, add.coord = FALSE) {
  if (!any(grepl("package:merra2sample", search()))) library("merra2sample")
  nms <- unique(tolower(month.abb[month]))
  nms <- paste0("merra2_", nms)
  x <- lapply(nms, get)
  x <- data.table::rbindlist(x)
  if (add.coord) x <- add_coord(x)
  return(x)
}

if (F) {
  merra2_sample()
  merra2_sample(2:3)
  merra2_sample(1:2, T)
}

