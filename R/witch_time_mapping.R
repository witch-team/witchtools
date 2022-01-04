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
  tmap <- read.csv(f, colClasses="numeric")
  #expand to yearly data table
  tmap <- setDT(tmap)[.(seq(min(begyear),max(endyear))), on = .(year)]
  #fill NAs
  tmap$unique <- 1
  for (x in names(tmap)) {
    tmap[is.na(get(x)), (x) := 
           tmap[!is.na(get(x))][.SD, on=.(unique), roll="nearest", get(paste0("x.",x))]]
  }
  tmap$unique <- NULL
  #change order
  setcolorder(tmap, c("t","year","refyear","pred","tperiod","begyear","endyear"))
  #all columns except year should be characters
  setDT(tmap); for (j in c("t","refyear","pred","tperiod","begyear","endyear")) set(tmap, j=j, value = as.character(tmap[[j]]))
  #pred of first node needs to be empty
  tmap[tperiod=="1"]$pred <- ""

  return(tmap)
}
