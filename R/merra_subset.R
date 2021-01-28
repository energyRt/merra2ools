#' Title
#'
#' @param year 
#' @param month 
#' @param day 
#' @param hour 
#' @param tz 
#'
#' @return
#' @export
#'
#' @examples
#' NA
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
    day <- days_in_month(ymd(paste(year, f(month), f(1), sep = "-")))
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
                              from = "1980-01-01 00", to = "2019-12-31 23", 
                              tz = "UTC", 
                              cols = NULL,
                              quiet = FALSE,
                              rows_lim = 2*10^9,
                              original.units = TRUE) {
  # browser()
  # tz <- "Pacific/Auckland"
  if (!any(grepl("package:fst", search()))) library("fst")
  
  op <- getOption("dplyr.summarise.inform")
  options(dplyr.summarise.inform = F)

  from <- lubridate::ymd_h(from, tz = tz) + lubridate::minutes(30L)
  from <- lubridate::with_tz(from, tzone = "UTC")
  
  to <- lubridate::ymd_h(to, tz = tz) + lubridate::minutes(30L)
  to <- lubridate::with_tz(to, tzone = "UTC")
  
  stopifnot(to >= from)
  
  from_to_h <- seq(from, to, by = "hour")
  
  yrs <- seq(lubridate::year(from), lubridate::year(to))
  yyyymm <- unique(format(seq(as.Date(from), as.Date(to), by = "day"), format = "%Y%m"))
  mr <- check_merra2(detailed = T)
  
  fls <- mr$merra_files[grepl(paste(yyyymm, collapse = "|"), mr$merra_files)]
  nrows_est <- 24 * 30 * length(locid) * length(fls)
  if (nrows_est > rows_lim) {
    message("The request might exeed rows' number limit ('rows_lim').\nTry to reduce the number of locations ('locid') and/or the time interval ('from' & 'to')")
    return(invisible(NULL))
  }
  sam <- NULL
  for (f in 1:length(fls)) {
    if (!quiet) cat("file:", fls[f])
    merra <- fst::read_fst(file.path(mr$path, fls[f]), as.data.table = T)
    if (!is.null(sam_i[["loc_id"]])) sam_i <- dplyr::rename(sam_i, locid = loc_id)
    if (!is.null(sam_i[["datetime"]])) sam_i <- dplyr::rename(sam_i, UTC = datetime)
    if (!is.null(cols)) sam_i <- sam_i[, ..cols]    
    ii <- merra$locid %in% locid 
    sam_i <- merra[ii,]
    ii <- sam_i$UTC %in% from_to_h
    sam_i <- sam_i[ii,]
    # browser()

    if (is.null(sam)) {
      sam <- sam_i
    } else {
      sam <- dplyr::bind_rows(sam, sam_i)
    }
    if (!quiet) {
      cat(", ", format(object.size(sam), units = "auto"), 
          ", nrow = ", nrow(sam), 
          ", ncol = ", ncol(sam), 
          "\n", sep = "")
    }
  }
  options(dplyr.summarise.inform = op)
  # browser()
  if (original.units) {
    if (!is.null(sam[["W10M"]])) sam[["W10M"]] <- sam[["W10M"]]/10
    if (!is.null(sam[["W50M"]])) sam[["W50M"]] <- sam[["W50M"]]/10
    if (!is.null(sam[["ALBEDO"]])) sam[["ALBEDO"]] <- sam[["ALBEDO"]]/100
  }
  return(sam)
}

if (F) {
  get_merra2_subset()
  lids <- sample(merra2_mar$locid, 10)
  merra_fl <- get_merra2_subset(
    locid = lids,
    from = "2000-01-14 00", to = "2000-01-15 23", tz = "America/New_York")
  
  merra_fl2 <- get_merra2_subset(
    locid = lids, cols = 1:3,
    from = "1990-01-01 00", to = "1990-03-31 23", tz = "America/New_York")

  merra_fl3 <- get_merra2_subset(
    locid = lids, cols = c("UTC", "locid", "T10M", "W10M", "W50M"),
    from = "1990-01-01 00", to = "1990-03-31 23", tz = "America/New_York")
  
  # write.fst(sam, path = file.path("tmp/merra2_subset.fst"), 100)  
  # merra2_mar <- sam
  merra2_mar <- get_merra2_subset(from = "2001-01-21 00", to = "2001-01-21 23")
  # save(merra2_mar, file = "data/merra2_mar.RData")
  save(merra2_mar, file = "tmp/merra2_mar_3d.RData")
  fst::write_fst(merra2_mar, path = "tmp/merra2_mar_3d.fst", 100)

  merra2_jan <- get_merra2_subset(from = "2010-01-21 00", to = "2010-01-21 23")
  merra2_feb2 <- get_merra2_subset(from = "2010-02-21 00", to = "2010-02-21 23")
  merra2_mar <- get_merra2_subset(from = "2010-03-21 00", to = "2010-03-21 23")
  merra2_apr <- get_merra2_subset(from = "2010-04-21 00", to = "2010-04-21 23")
  merra2_may <- get_merra2_subset(from = "2010-05-21 00", to = "2010-05-21 23")
  merra2_jun <- get_merra2_subset(from = "2010-06-21 00", to = "2010-06-21 23")
  merra2_jul <- get_merra2_subset(from = "2010-07-21 00", to = "2010-07-21 23")
  merra2_aug <- get_merra2_subset(from = "2010-08-21 00", to = "2010-08-21 23")
  merra2_sep <- get_merra2_subset(from = "2010-09-21 00", to = "2010-09-21 23")
  merra2_oct <- get_merra2_subset(from = "2010-10-21 00", to = "2010-10-21 23")
  merra2_nov <- get_merra2_subset(from = "2010-11-21 00", to = "2010-11-21 23")
  merra2_dec <- get_merra2_subset(from = "2010-12-21 00", to = "2010-12-21 23")
  
  save(merra2_jan, file = "data/merra2_jan.RData")
  save(merra2_feb, file = "data/merra2_feb.RData")
  save(merra2_mar, file = "data/merra2_mar.RData")
  save(merra2_apr, file = "data/merra2_apr.RData")
  save(merra2_may, file = "data/merra2_may.RData")
  save(merra2_jun, file = "data/merra2_jun.RData")
  save(merra2_jul, file = "data/merra2_jul.RData")
  save(merra2_aug, file = "data/merra2_aug.RData")
  save(merra2_sep, file = "data/merra2_sep.RData")
  save(merra2_oct, file = "data/merra2_oct.RData")
  save(merra2_nov, file = "data/merra2_nov.RData")
  save(merra2_dec, file = "data/merra2_dec.RData")
  
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
                            original.units = TRUE) {
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
  
  if (original.units) {
    if (!is.null(merra[["W10M"]])) merra[["W10M"]] <- merra[["W10M"]]/10
    if (!is.null(merra[["W50M"]])) merra[["W50M"]] <- merra[["W50M"]]/10
    if (!is.null(merra[["ALBEDO"]])) merra[["ALBEDO"]] <- merra[["ALBEDO"]]/100
  }
  return(merra)
}

if (F) {
  read_merra_file("200001")
}

#' Add longitude and latitude of 'locid'
#'
#' @param x 
#' @param force 
#'
#' @return
#' @export
#'
#' @examples
#' NA
add_coord <- function(x, force = FALSE) {
  if (is.null(x$locid)) stop("'x' should have 'locid' column")
  if (!is.null(x$lon) || !is.null(x$lat)) {
    if (force) {
      x$lon <- NULL
      x$lat <- NULL
    } else {
      stop("'x' already has 'lon' and/or 'lat' coordinates, use 'force = TRUE' to overwrite")
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

