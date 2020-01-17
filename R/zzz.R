#zzz.R
.onLoad <- function(libname, pkgname) {
  options(witchtools.method = "piggyback")
  options(witchtools.witch_data_repo = "witch-team/witch-data")
}
