#' Read region mapping from a regional WITCH GAMS file.
#'
#' \code{witch_region_mapping} reads a regional WITCH GAMS file and returns the
#' regional mapping as defined in the file. The function scans the GAMS file
#' with the assumptions that each iso3 is on one line and in a set named map_*.
#'
#' @param f Regional WITCH GAMS file
#'
#' @export
#' @examples
#' \dontrun{
#' witch_region_mapping('input/regions/witch17.inc')
#' }
#'
witch_region_mapping <- function(f){

  rif <- readLines(f)
  rif <- rif[rif != ""]                           # Remove empty lines
  rif <- rif[!stringr::str_detect(rif,"^\\*")]    # Remove * comments
  rif <- stringr::str_trim(stringr::str_split_fixed(rif,"#",2)[,1]) # Remove #

  if (length(rif[stringr::str_detect(rif,"set map_*")]) == 0) return(NULL)

  set.begin <- grep("set map_*",tolower(rif))[1]
  set.end <- set.begin + grep(";",rif[set.begin:length(rif)])[1]
  rim <- rif[(set.begin + 1):(set.end - 2)]
  rim <- stringr::str_split(rim,"\\.")
  rim <- data.table::data.table(matrix(unlist(rim), ncol = 2, byrow = TRUE))
  rim[,V1 := tolower(V1)]
  rim[,V2 := toupper(V2)]
  data.table::setnames(rim,c(stringr::str_sub(basename(f), 1, -5), "iso3"))

  return(rim)

}
