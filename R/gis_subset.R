#' Get MERRA-2 subset for
#'
#' @param from starting date and time of the subset in "YYYY-MM-DD HH" format
#' @param to ending date and time of the subset in "YYYY-MM-DD HH" format
#' @param tz time zone of the requested date-time interval (see <https://en.wikipedia.org/wiki/List_of_tz_database_time_zones>)
#' @param locid integer vector with location locid (values from 1 to 207936)
#' @param cols names or indexes of columns from the database (NULL default, returning all available columns)
#' @param quiet if process should be reported
#' @param rows_lim 
#' @param original.units 
#'
#' @return
#' data.table with the subset of 
#' @export
#'
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
  op <- getOption("dplyr.summarise.inform")
  options(dplyr.summarise.inform = F)
  # data_path <- get_merra2_dir()
  
  from <- lubridate::ymd_h(from, tz = tz) + lubridate::minutes(30L)
  from <- lubridate::with_tz(from, tzone = "UTC")
  
  to <- lubridate::ymd_h(to, tz = tz) + lubridate::minutes(30L)
  to <- lubridate::with_tz(to, tzone = "UTC")
  
  stopifnot(to >= from)
  
  from_to_h <- seq(from, to, by = "hour")
  
  yrs <- seq(lubridate::year(from), lubridate::year(to))
  yyyymm <- unique(format(seq(as.Date(from), as.Date(to), by = "day"), format = "%Y%m"))
  mr <- check_merra2(detailed = T)
  
  # if (is.null(mr$path)) {
  #   stop("`merra2` directory is not found, use `?set_merra2` for help")
  # }
  # if (!dir.exists(mr$path)) {
  #   stop("`merra2` directory '", mr$path,"' is not found, use `?set_merra2` for help")
  # }
  # if (is.null(mr$nfiles) || mr$nfiles == 0) {
  #   stop("`merra2` directory '", mr$path,"' doesn't have any MERRA-2 data files")
  # }
  
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
    ii <- merra$loc_id %in% locid 
    sam_i <- merra[ii,]
    ii <- sam_i$datetime %in% from_to_h
    sam_i <- sam_i[ii,]
    # browser()
    if (!is.null(sam_i[["loc_id"]])) sam_i <- dplyr::rename(sam_i, locid = loc_id)
    if (!is.null(cols)) sam_i <- sam_i[, ..cols]
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
    if (!is.null(sam[["T10M"]])) sam[["T10M"]] <- sam[["T10M"]]/10
    if (!is.null(sam[["W10M"]])) sam[["W10M"]] <- sam[["W10M"]]/10
    if (!is.null(sam[["W50M"]])) sam[["W50M"]] <- sam[["W50M"]]/10
    if (!is.null(sam[["ALBEDO"]])) sam[["ALBEDO"]] <- sam[["ALBEDO"]]/100
  }
  return(sam)
}

if (F) {
  get_merra2_subset()
  lids <- sample(merra2_sample$locid, 10)
  merra_fl <- get_merra2_subset(
    locid = lids,
    from = "2000-01-14 00", to = "2000-01-15 23", tz = "America/New_York")
  
  merra_fl2 <- get_merra2_subset(
    locid = lids, cols = 1:3,
    from = "1990-01-01 00", to = "1990-03-31 23", tz = "America/New_York")

  merra_fl3 <- get_merra2_subset(
    locid = lids, cols = c("datetime", "locid", "T10M", "W10M", "W50M"),
    from = "1990-01-01 00", to = "1990-03-31 23", tz = "America/New_York")
  
  # write.fst(sam, path = file.path("tmp/merra2_subset.fst"), 100)  
  # merra2_sample <- sam
  merra2_sample <- get_merra2_subset(from = "2001-01-21 00", to = "2001-01-21 23")
  # save(merra2_sample, file = "data/merra2_sample.RData")
  save(merra2_sample, file = "tmp/merra2_sample_3d.RData")
  fst::write_fst(merra2_sample, path = "tmp/merra2_sample_3d.fst", 100)

  merra2_jan <- get_merra2_subset(from = "2010-01-21 00", to = "2010-01-21 23")
  merra2_feb <- get_merra2_subset(from = "2010-02-21 00", to = "2010-02-21 23")
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
