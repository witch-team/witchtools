#' Handler for witch-data files in WITCH
#'
#' Returns the location of a file from the witch-data repository for processing,
#' after downloading it from github if necessary using the piggyback library.
#' @param file Name of the file in the with-data repository
#' @param version Release version of the file (required for method piggyback)
#' @param idir directory to read/download files
#' @param method 'piggyback' or 'witch-data'.
#' @param noCheck For the piggyback method, don't check and download the file.
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
                       noCheck = getOption("witchtools.noCheck", FALSE),
                       repo = getOption("witchtools.witch_data_repo"),
                       remote = getOption("witchtools.witch_data_remote")) {

  # Check method name
  if (!method %in% c("piggyback", "witch-data", "dvc")) {
    warning(paste("Method", method, "does not exist."))
  }

  # default values for idir
  if (method == "piggyback" & is.null(idir)) {
    idir <- normalizePath(file.path("input", "data"), mustWork = TRUE)
  }
  if (method == "dvc" & is.null(idir)) {
    idir <- normalizePath(file.path("input", "data"), mustWork = TRUE)
  }
  if (method == "witch-data" & is.null(idir)) {
    idir <- normalizePath(file.path("..", "witch-data"), mustWork = TRUE)
  }

  if (!dir.exists(idir)) {
    stop(paste("Directory", idir, "does not exist."))
  }

  if (method == "dvc") {
    file <- stringr::str_replace_all(file, "/", "-")

    if (!noCheck) {
      if (is.null(remote)) {
        cmd = paste0("dvc pull '",normalizePath(file.path(idir,paste0(file,".dvc")), mustWork = FALSE),"'")
      } else {
        cmd = paste0("dvc pull -r ",remote," '",normalizePath(file.path(idir,paste0(file,'.dvc')), mustWork = FALSE),"'")
      }
      system(cmd)
    }
  }

  if (method == "piggyback") {
    file <- stringr::str_replace_all(file, "/", "-")

    if (!noCheck) {
      piggyback::pb_download(repo = repo, tag = version, file = file, dest = idir)
    }
  }

  return(file.path(idir, file))
}


#' Upload a file as an asset in a release of a github repository.
#' @param file Name of the file in the with-data repository
#' @param version Release version of the file (without space)
#' @param method 'piggyback'.
#' @param repo github repository name
#'
#' @export
#' @examples
#' \dontrun{
#' witch_data_upload("ssp/ssp_population.csv", version = "v0.0.1")
#' }
witch_data_upload <- function(file, version = NULL,
                              method = getOption("witchtools.method"),
                              repo = getOption("witchtools.witch_data_repo")) {

  # Check method name
  if (!method %in% c("piggyback", "witch-data", "dvc")) {
    warning(paste("Method", method, "does not exist."))
  }

  if (method == "piggyback") {
    if (is.null(version)) stop("version cannot be NULL")

    try(piggyback::pb_new_release(repo = repo, tag = version), silent = TRUE)
    piggyback::pb_upload(file,
      name = stringr::str_replace_all(file, "/", "-"),
      tag = version
    )
  }

  if (method == "dvc") {

    system(paste0("dvc add '",normalizePath(file, mustWork = FALSE),"'"))
    system(paste0("dvc push '",normalizePath(paste0(file,".dvc"), mustWork = FALSE),"'"))

  }

}
