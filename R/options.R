.onLoad <- function(libname, pkgname) {
  options(merra2db = path.expand("~/data/merra2_subset"))
}

#' Get, Set, or Check content of the Directory with the MERRA2 database Subset
#'
#' @description
#' \code{merra2db} is a subset of MERRA2 database with global
#'
#' \code{get_merra2db} returns the current filepath to the MERRA2 data subset;
#'
#' \code{set_merra2db} is used to set the directory of the database;
#'
#' \code{check_merra2db} checks the content of the \code{merra2db} directory, and lists data-files.
#'
#' @param path A character string of the path (default "~/data/merra2_subset")
#' @param detailed logical, if a list of files should be returned.
#'
#' @return
#' \code{get_merra2db} returns a character string or NULL if the directory is not set.
#'
#' \code{set_merra2db} returns the currently set directory before the change, invisibly.
#'
#' \code{check_merra2db} returns number or list of data-files in the \code{merra2db} directory.
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
#' @name merra2db
NULL

#' @export
#' @rdname merra2db
get_merra2db <- function() {
  getOption("merra2db")
}

#' @export
#' @rdname merra2db
set_merra2db <- function(path) {
  x <- getOption("merra2db")
  options(merra2db = path)
  invisible(x)
}


#' Check if MERRA2 data-files exist in provided directory
#'
#' @export
#' @rdname merra2db
check_merra2db <- function(path = getOption("merra2db"), detailed = FALSE) {
  if (!dir.exists(path)) {
    message("The dirrectory ", path, " doesn't exists")
    return(F)
  } else {
    lst <- list(path = path)
  }
  fls <- list.files(path, pattern = "merra2_[12][90][890123][0-9][01][0-9].fst")
  locid <- file.exists(file.path(path, "loc_id.RData"))

  if (!detailed) {
    return(length(fls))
  }

  return(
    c(lst, list(
      nfiles = length(fls),
      merra_files = fls,
      loc_id = locid
    ))
  )
}

if (F) {
  check_merra2db()
  check_merra2db(detailed = T)
}

#' MERRA2 database subset info
#'
#' @export
#' @rdname merra2db
merra2db <- function() {
  print("merra2db info...")
  # subset
  
  # files
  
}

# a filepath to the currently set directory of MERRA2 database
# sets path for MERRA2-subset database
