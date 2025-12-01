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
      message("Installing gdxtools from GitHub...")
      remotes::install_github("lolow/gdxtools", quiet = !interactive())
    }
  }

  # Special handling for duckdb (uses r-universe)
  if ("duckdb" %in% pkgs) {
    if (!rlang::is_installed("duckdb")) {
      install_duckdb_safe()
    }
  }

  # Handle remaining packages
  other_pkgs <- setdiff(pkgs, c("gdxtools", "duckdb"))
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

  repos <- c(CRAN = "https://cloud.r-project.org")

  message("Installing package '", pkg, "' from CRAN...")

  utils::install.packages(pkg, repos = repos)

  # Verify the package is now installed
  if (!rlang::is_installed(pkg)) {
    stop(
      "Package '", pkg, "' installation completed but the package is not available.\n",
      "This may indicate a failed installation. Please try installing manually with:\n",
      "  install.packages('", pkg, "')",
      call. = FALSE
    )
  }

  message("Package '", pkg, "' installed successfully.")
}

#' Install duckdb package with proper handling
#'
#' @keywords internal
install_duckdb_safe <- function() {
  pkg <- "duckdb"

  # Use r-universe for duckdb installation for better pre-compiled binaries
  repos <- c("https://duckdb.r-universe.dev", "https://cloud.r-project.org")

  message("Installing package '", pkg, "' from r-universe and CRAN...")

  utils::install.packages(pkg, repos = repos)

  # Verify the package is now installed
  if (!rlang::is_installed(pkg)) {
    stop(
      "Package '", pkg, "' installation completed but the package is not available.\n",
      "This may indicate a failed installation. Please try installing manually with:\n",
      "  install.packages('", pkg, "', repos = c('https://duckdb.r-universe.dev', 'https://cloud.r-project.org'))",
      call. = FALSE
    )
  }

  message("Package '", pkg, "' installed successfully.")
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
