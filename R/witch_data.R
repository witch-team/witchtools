#' Handler for witch-data files in WITCH
#'
#' Returns the location of a file from the witch-data repository for processing,
#' method (`local`) or after checking/downloading it from the DVC remote drive
#' (method `dvc`).
#' @param file Name of the file in the with-data repository
#' @param version File version, only required for method piggyback (deprecated)
#' @param idir directory to read/download files
#' @param method 'dvc' or 'local'.
#' @param noCheck don't check and download the file.
#' @param repo github repository name
#' @param remote dvc remote storage
#'
#' @export
#' @examples
#' \dontrun{
#' witch_data("ssp/ssp_population.csv", version = "v0.0.1")
#' witch_data("ssp/ssp_population.csv", method = "witch-data")
#' witch_data("ssp/ssp_population.csv", noCheck = TRUE)
#' }
witch_data <- function(file, version = NULL,
                       idir = getOption("witchtools.idir"),
                       method = getOption("witchtools.method"),
                       noCheck = getOption("witchtools.noCheck", TRUE),
                       repo = getOption("witchtools.witch_data_repo"),
                       remote = getOption("witchtools.witch_data_remote")) {

  # default values for idir
  idir <- normalizePath(file.path("input", "data"), mustWork = TRUE)

  if (method == "witch-data" & is.null(idir)) {
    idir <- normalizePath(file.path("..", "witch-data"), mustWork = TRUE)
  }

  if (!dir.exists(idir)) {
    stop(paste("Directory", idir, "does not exist."))
  }

  file <- stringr::str_replace_all(file, "/", "-")

  if (method == "dvc") {

    if (!noCheck) {
      if (is.null(remote)) {
        cmd = paste0('dvc pull "',normalizePath(file.path(idir,paste0(file,".dvc")), mustWork = FALSE),'"')
      } else {
        cmd = paste0('dvc pull -r ',remote,' "',normalizePath(file.path(idir,paste0(file,'.dvc')), mustWork = FALSE),'"')
      }
      res <- try(system(cmd))
      stopifnot(res == 0)
    }
  }

  return(file.path(idir, file))
}


#' Upload a file as an asset in a release of a github repository (deprecated).
#' @param file Name of the file in the with-data repository
#' @param version NULL
#' @param method NULL.
#' @param repo NULL
#'
#' @export
#' @examples
#' \dontrun{
#' witch_data_upload("ssp/ssp_population.csv", version = "v0.0.1")
#' }
witch_data_upload <- function(file, version = NULL,
                              method = NULL,
                              repo = NULL) {

  warning("`witch_data_upload` is deprecated as of witchtools 0.4.1.")

}
