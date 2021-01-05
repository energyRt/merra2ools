.onLoad <- function(libname, pkgname) {
  if (file.exists("~/.merra2ools")) {
    source("~/.merra2ools")
  } else {
    warning("MERRA2 data directory is not found.\n
            Use '?set_merra' for help")
  }
  # options(merra2 = path.expand("data/merra2_subset"))
  options(merra2.verbose = TRUE)
}

#' Get, Set, or Check content of the Directory with the MERRA2 database Subset
#'
#' @description
#' \code{merra2} is a subset of MERRA2 database with global
#'
#' \code{get_merra2} returns the current filepath to the MERRA2 data subset;
#'
#' \code{set_merra2} is used to set the directory of the database;
#'
#' \code{check_merra2} checks the content of the \code{merra2} directory, and lists data-files.
#'
#' @param path A character string of the path (default "~/data/merra2_subset")
#' @param detailed logical, if a list of files should be returned.
#'
#' @return
#' \code{get_merra2} returns a character string or NULL if the directory is not set.
#'
#' \code{set_merra2} returns the currently set directory before the change, invisibly.
#'
#' \code{check_merra2} returns number or list of data-files in the \code{merra2} directory.
#'
#' @references
#' Bosilovich, M. G., R. Lucchesi, and M. Suarez, 2016: MERRA-2: File Specification.
#' GMAO Office Note No. 9 (Version 1.1), 73 pp,
#' available from http://gmao.gsfc.nasa.gov/pubs/office_notes
#'
#' Global Modeling and Assimilation Office (GMAO) (2015), MERRA-2 tavg1_2d_rad_Nx: 2d,1-Hourly,Time-Averaged,Single-Level,Assimilation,Radiation Diagnostics 0.625 x 0.5 degree V5.12.4 (M2T1NXRAD) at GES DISC. Accessed: 2019-2020 at DOI: 10.5067/Q9QMY5PBNV1T
#'
#' Global Modeling and Assimilation Office (GMAO) (2015), MERRA-2 tavg1_2d_slv_Nx: 2d,1-Hourly,Time-Averaged,Single-Level,Assimilation,Single-Level Diagnostics V5.12.4 (M2T1NXSLV) at GES DISC. Accessed: 2019-2020 at DOI: 10.5067/VJAFPLI1CSIV
#'
#' @name merra2
NULL

#' @export
#' @rdname merra2
get_merra2_dir <- function() {
  getOption("merra2.dir")
}

#' @export
#' @rdname merra2
set_merra2_options <- function(merra2.dir = NULL, pwatts.api.key = NULL) {
  # browser()
  if (!is.null(merra2.dir)) {
    if (!dir.exists(merra2.dir)) stop("Directory '", merra2.dir, "' does not exist")
    if (!check_merra2(path = merra2.dir)) {
      warning("Directory '", merra2.dir, "' does not have MERRA-2 data")
    }
  }
  if (is.null(merra2.dir)) merra2.dir <- getOption("merra2.dir")
  if (is.null(pwatts.api.key)) pwatts.api.key <- getOption("pwatts.api.key")
  options(merra2.dir = merra2.dir)
  options(pwatts.api.key = pwatts.api.key)
  # x <- getOption("merra2")
  con <- file("~/.merra2ools")
  writeLines(c(
    paste0("options(merra2.dir = '", merra2.dir, "')"),
    paste0("options(pwatts.api.key = '", pwatts.api.key, "')")),
    con)
  close(con)
  # invisible(x)
}

if (F) {
  set_merra2_options()
}

#' Check if MERRA2 data-files exist in provided directory
#'
#' @export
#' @rdname merra2
check_merra2 <- function(path = get_merra2_dir(), detailed = FALSE,
                         verbose = getOption("merra2.verbose")) {
  if (is.null(path) || is.na(path)) return(FALSE)
  if (!dir.exists(path)) {
    message("The dirrectory ", path, " doesn't exists")
    return(F)
  } else {
    lst <- list(path = path)
  }
  fls <- list.files(path, pattern = "merra2_[12][90][890123][0-9][01][0-9].fst")
  locid <- file.exists(file.path(path, "locid.RData"))

  if (!detailed) {
    if (verbose) cat(length(fls), "MERRA-2 files found")
    return(invisible(length(fls)))
  }

  return(
    c(lst, list(
      nfiles = length(fls),
      merra_files = fls,
      locid = locid
    ))
  )
}

if (F) {
  check_merra2()
  check_merra2(detailed = T)
}

#' MERRA2 database subset info
#'
#' @export
#' @rdname merra2
merra2_info <- function() {
  print("merra2 info...")
  # subset
  
  # files
  
}

