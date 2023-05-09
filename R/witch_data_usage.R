#' Wrapper for the usage of a make_data file in WITCH for future use of witch_data
#'
#' Returns the list containing the options as returned by docopt, the method
#' name `method` and the location of the input data directory. It also updates
#' the witchtools options, according to the call parameters.
#' @param file Name of the make data file
#'
#' @export
#' @examples
#' \dontrun{
#' opts <- witch_data_usage("make_data_validation.R")
#' }
witch_data_usage <- function(file) {

  doc <- stringr::str_glue('usage: {file} [-w witch_dir] [-i <input-data-dir>] [-m <method>] [--noCheck]
options:
-w <dir>    witch directory
-m <method> method to access files (default: local). Deprecated.
-i <dir>    input data directory, where data are downloaded
--noCheck   not checking data files')
  opts <- docopt::docopt(doc)

  opts[["witch_dir"]] = ifelse(is.null(opts[["w"]]), here::here(), as.character(opts["w"]))
  opts[["idir"]] = ifelse(is.null(opts[["i"]]), here::here("input","data"), as.character(opts["i"]))
  opts[["method"]] = ifelse(is.null(opts[["m"]]), "local", as.character(opts["m"]))
  options(witchtools.idir = opts[["idir"]])
  options(witchtools.method = opts[["method"]])
  options(witchtools.noCheck = opts[["noCheck"]])

  return(opts)

}
