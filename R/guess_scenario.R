#' Guess scenario names from a list of results gdx files.
#'
#' Return a vector with the scenario names. The scenario
#' is obtained from the name of the gdx without 'results_' and the extension.
#' If the files are located in different directory
#'
#' @param gdxlist list of results gdx files
#' @param keep_ssp if TRUE, keep the ssp in the name even if it the same across
#' scenario
#' @param sep separator between the directory prefix and the name, if any.
#' @export
#'
guess_scenario <- function(gdxlist, keep_ssp = FALSE, sep = "_") {

  # Check if all files are in the same directory.
  same_path <- length(unique(dirname(gdxlist))) == 1

  # If different directories, extract
  if (!same_path) {
    npath <- normalizePath(dirname(gdxlist), winslash = "/", mustWork = FALSE)
    spath <- strsplit(npath, "/")
    lpath <- unlist(lapply(spath, function(x) x[[length(x)]]))
    lpath <- tolower(lpath)
  }

  # Basic cleaning of the filename
  fname <- basename(gdxlist)
  fname <- tolower(fname)
  fname <- stringr::str_remove(fname, ".gdx")
  fname <- stringr::str_remove(fname, "results_")

  # Should we remove ssp?
  if(!keep_ssp) {
    ssp <- stringr::str_extract(fname, "ssp\\d")
    if (length(unique(ssp)) == 1) {
      fname <- stringr::str_remove(fname, paste0(ssp[1], '_'))
    }
  }

  # Add prefix if different directories
  if (!same_path) {
    fname <- paste(lpath, fname, sep = sep)
  }

  # Provide a warning if duplicated scenario name
  if (anyDuplicated(fname) > 0 ) {
    warning(paste('Duplicated scenario names for different gdx.',
            'Please consider to manually provide scenario names'))
  }

  return(fname)

}
