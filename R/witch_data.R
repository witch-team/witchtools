#' Returns the location of a file from the witch-data repository for processing,
#' after downloading it from github if necessary using the piggyback library.
#' @param file Name of the file in the with-data repository
#' @param version Release version of the file (required for method piggyback)
#' @param method 'piggyback' or 'repository'.
#' @param noCheck For the piggyback method, don't check and download the file.
#' @param repo github repository name
#'
#' @export
#' @examples
#' \dontrun{
#' witch_data("ssp/ssp_population.csv", version = 'v0.0.1')
#' witch_data("ssp/ssp_population.csv", method = 'witch-data')
#' witch_data("ssp/ssp_population.csv", noCheck = TRUE)
#' }
witch_data <- function(file, version = NULL,
                       idir = getOption('witchtools.idir'),
                       method = getOption('witchtools.method'),
                       noCheck = getOption('witchtools.noCheck',FALSE),
                       repo = getOption('witchtools.witch_data_repo')) {

  # Check method name
  if (!method %in% c("piggyback","witch-data")) {
    warning(paste("Method", method, "does not exist."))
  }

  # default values for idir
  if (method == "piggyback" & is.null(idir)) {
    idir = normalizePath(file.path("input","data"))
  }
  if (method == "witch-data" & is.null(idir)) {
    idir = normalizePath(file.path("..","witch-data"))
  }

  if (!dir.exists(idir)) {
    stop(paste("Directory", idir, "does not exist."))
  }

  if (method == "piggyback") {
    file = stringr::str_replace(file,"/","-")

    if (!noCheck) {
      piggyback::pb_download(repo = repo,tag = version,file = file, dest = idir)
    }

  }

  return(file.path(idir,file))

}
