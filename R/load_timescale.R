#' Load timescale mapping from a timescale CSV file.
#'
#' @param f times WITCH GAMS file
#'
#' @export
#' @import data.table
#' @examples
#' \dontrun{
#' load_timescale_mapping('input/time/t30.inc')
#' }
#'
load_timescale_mapping = function(f){
  tab = data.table::fread(f,colClasses = "character")
  setnames(tab,'year','refyear')
  # Expand year
  tab = data.table::rbindlist(lapply(1:nrow(tab), function(i){tab[i,.(t,
                                                              year = begyear:endyear,
                                                              refyear,
                                                              pred,
                                                              tperiod,
                                                              begyear,
                                                              endyear)]}))
  tab[,year := as.numeric(year)]
  return(tab)
}
