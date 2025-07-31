#' Install, if necessary, and load R libraries
#'
#' @param pkgs package names as a character vector
#' @param loading if TRUE, package is loaded
#'
#' @export
#' @examples
#' \dontrun{
#' require_package(c("data.table","gdxtools"), loading = FALSE)
#' }
require_package <- function(pkgs, loading = TRUE) {
  if ("gdxtools" %in% pkgs) {
    if (! rlang::is_installed("gdxtools")) {
      rlang::check_installed("remotes")
      remotes::install_github("lolow/gdxtools")
    }
    pkgs <- setdiff(pkgs, "gdxtools")
  }
  rlang::check_installed(pkgs)
  if (loading) {
    for (pkg in pkgs) {
      suppressPackageStartupMessages(library(pkg,
                                            character.only = TRUE,
                                            quietly = TRUE
                                            )
                                     )
    }
  }
}


#' Install, if necessary, and load the gdxtools library
#'
#' @param loading if TRUE, package is loaded
#'
#' @export
#' @examples
#' \dontrun{
#' require_gdxtools()
#' }
require_gdxtools <- function(loading = TRUE) {
  require_package("gdxtools", loading = loading)
}
