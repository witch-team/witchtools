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
  # Special handling for gdxtools (installed from GitHub)
  if ("gdxtools" %in% pkgs) {
    if (!rlang::is_installed("gdxtools")) {
      # First ensure remotes is available
      if (!rlang::is_installed("remotes")) {
        install_package_safe("remotes")
      }
      
      # Install gdxtools from GitHub
      if (interactive()) {
        message("Installing gdxtools from GitHub...")
      }
      remotes::install_github("lolow/gdxtools", quiet = !interactive())
    }
  }
  
  # Handle remaining packages
  other_pkgs <- setdiff(pkgs, "gdxtools")
  for (pkg in other_pkgs) {
    if (!rlang::is_installed(pkg)) {
      install_package_safe(pkg)
    }
  }
  
  # Load packages if requested
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

#' Install a package with proper handling of non-interactive sessions
#'
#' @param pkg package name as a character string
#' @keywords internal
install_package_safe <- function(pkg) {
  # Check if user library path exists and is writable
  user_lib <- Sys.getenv("R_LIBS_USER")
  
  # Get the first writable library path
  lib_paths <- .libPaths()
  writable_lib <- NULL
  
  for (lib in lib_paths) {
    if (dir.exists(lib) && file.access(lib, mode = 2) == 0) {
      writable_lib <- lib
      break
    }
  }
  
  # If no writable library exists, check if we can create the user library
  if (is.null(writable_lib)) {
    if (user_lib != "" && !dir.exists(user_lib)) {
      # Try to create user library directory
      tryCatch({
        dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
        writable_lib <- user_lib
      }, error = function(e) {
        writable_lib <- NULL
      })
    }
    
    # If still no writable library, provide helpful error
    if (is.null(writable_lib)) {
      stop(
        "No writable R library directory found to install package '", pkg, "'.\n",
        "Please create a user library directory by running:\n",
        "  dir.create(Sys.getenv('R_LIBS_USER'), recursive = TRUE)\n",
        "Or set R_LIBS_USER environment variable to a writable directory:\n",
        "  Sys.setenv(R_LIBS_USER = '/path/to/your/R/library')\n",
        "Then restart R and try again.",
        call. = FALSE
      )
    }
  }
  
  # Install the package
  # Ensure we have a valid repository setting
  repos <- getOption("repos")
  if (is.null(repos) || identical(repos, c(CRAN = "@CRAN@")) || repos["CRAN"] == "@CRAN@") {
    repos <- c(CRAN = "https://cloud.r-project.org")
  }
  
  if (interactive()) {
    message("Installing package: ", pkg)
    utils::install.packages(pkg, lib = writable_lib, quiet = FALSE, repos = repos)
  } else {
    # Non-interactive: install without prompting
    utils::install.packages(pkg, lib = writable_lib, quiet = TRUE, repos = repos)
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
