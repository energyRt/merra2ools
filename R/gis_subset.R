#' Get MERRA-2 subset for
#'
#' @param from starting date and time of the subset in "YYYY-MM-DD HH" format
#' @param to ending date and time of the subset in "YYYY-MM-DD HH" format
#' @param tz time zone of the requested date-time interval (see <https://en.wikipedia.org/wiki/List_of_tz_database_time_zones>)
#' @param IDs integer vector with location IDs (values from 1 to 207936)
#' @param cols names or indexes of columns from the database (NULL default, returning all available columns)
#' @param quiet if process should be reported
#' @param rows_lim 
#'
#' @return
#' data.table with the subset of 
#' @export
#'
#' @examples
#' NA
get_merra2_subset <- function(IDs, 
                              from = "1980-01-01 00", to = "2019-12-31 23", 
                              tz = "UTC", 
                              cols = NULL,
                              quiet = FALSE,
                              rows_lim = 2*10^9) {
  # tz <- "Pacific/Auckland"
  op <- getOption("dplyr.summarise.inform")
  options(dplyr.summarise.inform = F)
  data_path <- getOption("merra2")
  
  from <- lubridate::ymd_h(from, tz = tz) + lubridate::minutes(30L)
  from <- lubridate::with_tz(from, tzone = "UTC")
  
  to <- lubridate::ymd_h(to, tz = tz) + lubridate::minutes(30L)
  to <- lubridate::with_tz(to, tzone = "UTC")
  
  stopifnot(from <= to)
  
  from_to_h <- seq(from, to, by = "hour")
  
  yrs <- seq(lubridate::year(from), lubridate::year(to))
  yyyymm <- unique(format(seq(as.Date(from), as.Date(to), by = "day"), format = "%Y%m"))
  mr <- check_merra2(detailed = T)
  
  if (is.null(mr$path)) {
    stop("`merra2` directory is not found, use `?set_merra2` for help")
  }
  if (!dir.exists(mr$path)) {
    stop("`merra2` directory '", mr$path,"' is not found, use `?set_merra2` for help")
  }
  if (is.null(mr$nfiles) || mr$nfiles == 0) {
    stop("`merra2` directory '", mr$path,"' doesn't have any MERRA-2 data files")
  }
  fls <- mr$merra_files[grepl(paste(yyyymm, collapse = "|"), mr$merra_files)]
  gagg <- NULL
  for (f in 1:length(fls)) {
    if (!quiet) cat("file:", fls[f])
    merra <- read_fst(file.path(mr$path, fls[f]), as.data.table = T)
    ii <- merra$locid %in% IDs 
    agg <- merra[ii,]
    ii <- agg$datetime %in% from_to_h
    agg <- agg[ii,]
    # browser()
    if (!is.null(cols)) agg <- agg[, ..cols]
    if (is.null(gagg)) {
      gagg <- agg
    } else {
      gagg <- bind_rows(gagg, agg)
    }
    if (!quiet) {
      cat(", ", format(object.size(gagg), units = "auto"), 
          ", nrow = ", nrow(gagg), 
          ", ncol = ", ncol(gagg), 
          "\n", sep = "")
    }
  }
  options(dplyr.summarise.inform = op)
  return(gagg)
}

if (F) {
  merra_fl <- get_merra2_subset(
    IDs = lids,
    from = "2000-01-14 00", to = "2000-01-15 23", tz = "America/New_York")
  
  merra_fl2 <- get_merra2_subset(
    IDs = lids, cols = 1:3,
    from = "1990-01-01 00", to = "1990-03-31 23", tz = "America/New_York")

  merra_fl3 <- get_merra2_subset(
    IDs = lids, cols = c("datetime", "locid", "T10M", "W10M", "W50M"),
    from = "1990-01-01 00", to = "1990-03-31 23", tz = "America/New_York")
  
  write.fst(gagg, path = file.path("tmp/merra2_subset.fst"), 100)  
  merra2_sample <- gagg
  save(merra2_sample, file = "data/merra2_sample.RData")
  
}
