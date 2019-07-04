#' Install, if necessary, and load a R library
#'
#' @param package package name
#' @param loading if TRUE, package is loaded
#'
#' @export
#' @examples
#' \dontrun{
#' require_package("devtools", loading = F)
#' }
require_package <- function(package, loading = T){
  if (!package %in% rownames(installed.packages())) {
    if (package == 'gdxtools') {
      require_package("devtools", loading = F)
      devtools::install_github('lolow/gdxtools')
    } else {
      try(install.packages(package, repos = "http://cran.rstudio.com"), silent = TRUE)
    }
  }
  if (loading) {
    suppressPackageStartupMessages(library(package,character.only = T, quietly = TRUE))
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
require_gdxtools <- function(loading = T){
  require_package('gdxtools', loading = loading)
}
