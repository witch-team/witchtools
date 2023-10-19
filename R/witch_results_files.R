#' List of WITCH results files
#'
#' Returns a vector of WITCH results files. Inspired by witch-plot source code.
#'
#' @family WITCH helper functions
#' @seealso \link{witch_query}.
#'
#' @param search_path Path where to start the search of the results files (default="^results_").
#' @param restrict vector of filtering patterns on the file names.
#' @param normalize if TRUE, return canonical form of the file names.
#' @param recursive if TRUE, search also in the sub directories.
#' @export
#'
witch_results_files <- function(search_path,
                                restrict = "^results_",
                                normalize = FALSE,
                                recursive = FALSE) {

  # List all gdx files
  filelist = list.files(path = search_path,
                        full.names = FALSE,
                        pattern = "*.gdx",
                        recursive = recursive)

  if (normalize) {
    filelist <- normalizePath(filelist)
  }

  # Clean filtering rules
  restrict <- restrict[restrict != ""]

  # Filter files
  filteredlist <- NULL
  for(pattern in restrict) {
    ff <- filelist[stringr::str_detect(basename(filelist), pattern)]
    filteredlist <- c(filteredlist, ff)
  }

  return(unique(filteredlist))

}
