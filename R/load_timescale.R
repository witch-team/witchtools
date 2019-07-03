# Load time mapping
#' @import data.table
#' @export
load_timescale_mapping = function(file){
  tab = data.table::fread(file,colClasses = "character")
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
