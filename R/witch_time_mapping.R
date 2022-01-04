#' Read time mapping from a csv file.
#'
#' \code{witch_time_mapping} reads a time csv file and returns the
#' time mapping as defined in the file. The function scans the csv file.
#'
#' @param f time mapping csv file
#'
#' @export
#' @examples
#' \dontrun{
#' witch_time_mapping("input/time/t30.csv")
#' }
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
