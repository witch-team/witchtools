#zzz.R
.onLoad <- function(libname, pkgname) {
  options(witchtools.method = "piggyback")
  options(witchtools.witch_data_repo = "witch-team/witch-data")
}

# Import package operators
#' @importFrom data.table ":=" "%like%" "%between%"

# Make sure data.table knows we know we're using it
.datatable.aware = TRUE

# Prevent R CMD check from complaining about the use of pipe expressions
# standard data.table variables
if (getRversion() >= "2.15.1")
  utils::globalVariables(c(".", ".I", ".N", ".SD"), utils::packageName())
