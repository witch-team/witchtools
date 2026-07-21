#' Read time mapping from a csv file.
#'
#' \code{witch_time_mapping} reads a time csv file and returns the
#' time mapping as defined in the file. The function scans the csv file.
#'
#' @param f time mapping csv file
#'
#' @returns A \code{data.table} with one row per (t, year) pair and the columns
#' \code{t}, \code{year} (numeric), \code{refyear}, \code{pred},
#' \code{tperiod}, \code{begyear} and \code{endyear}.
#'
#' @export
#' @examples
#' # Read the t30 time mapping shipped with the package
#' witch_time_mapping(
#'   system.file("extdata", "t30.csv", package = "witchtools")
#' )
#'
witch_time_mapping <- function(f) {
  tab <- data.table::fread(f, colClasses = "character")
  data.table::setnames(tab, "year", "refyear")
  # Expand year
  tab <- data.table::rbindlist(lapply(
    seq_len(nrow(tab)),
    function(i) {
      tab[i, .(t,
               year = begyear:endyear,
               refyear,
               pred,
               tperiod,
               begyear,
               endyear
      )]
    }
  ))
  tab[, year := as.numeric(year)]
  return(tab)
}
