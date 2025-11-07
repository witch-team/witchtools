# zzz.R
.onLoad <- function(libname, pkgname) {
  options(witchtools.method = "local")
  options(witchtools.witch_data_repo = "witch-team/witch-data")
  options(witchtools.noCheck = TRUE)
  
  # DuckDB optimization options
  options(witchtools.duckdb.memory_limit = NULL)  # e.g., "4GB", "8GB"
  options(witchtools.duckdb.streaming = FALSE)     # Use streaming for large tables
  options(witchtools.duckdb.chunk_size = 100000)   # Rows per chunk in streaming mode
  options(witchtools.duckdb.temp_directory = NULL) # Temp directory for disk spillover
}

# Import package operators
#' @importFrom data.table ":=" "%like%" "%between%"

# Make sure data.table knows we know we're using it
.datatable.aware <- TRUE

# Prevent R CMD check from complaining about the use of pipe expressions
# standard data.table variables
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(".", ".I", ".N", ".SD"), utils::packageName())
}
