# default loading (from R CRAN)
#' @export
require_package <- function(package, use_pak = F){
  if (use_pak) {
    if (!require(pak)) {
      install.packages("pak", repos = "https://cloud.r-project.org", dependencies = T)
      if (!require(pak)) stop("Package pak not found")
    }
  }
  if (!is.element(package, .packages(all.available = TRUE))) {
    if (use_pak) {
      pak::pkg_install(package,ask = F) 
    } else {
      try(install.packages(package, repos = "http://cran.rstudio.com"), silent = TRUE)
    }
  }
  suppressPackageStartupMessages(library(package,character.only = T, quietly = TRUE))  
}

#' @export
require_gdxtools <- function(use_pak = F){
  # gdxtools
  if (!is.element("gdxtools", .packages(all.available = TRUE))) {
    require_package("devtools", use_pak)
    if (use_pak) {
      pak::pkg_install('lolow/gdxtools',ask = F) 
    } else {
      install_github('lolow/gdxtools')
    }
  }
  if (packageVersion("gdxtools") < numeric_version("0.4.0")) {
    stop("You need to install a newer version of gdxtools (>=0.4.0). Please run remove.packages('gdxtools'), restart R and rerun this script.")
  }
  suppressPackageStartupMessages(library(gdxtools, quietly = TRUE))
}
