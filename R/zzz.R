# zzz.R

# Set package options, without clobbering any the user has already set.
.onLoad <- function(libname, pkgname) {
  op <- options()
  op_witchtools <- list(
    witchtools.method = "local",
    witchtools.witch_data_repo = "witch-team/witch-data",
    witchtools.noCheck = TRUE
  )
  toset <- !(names(op_witchtools) %in% names(op))
  if (any(toset)) {
    options(op_witchtools[toset])
  }
  invisible()
}

# Make sure data.table knows we know we're using it
.datatable.aware <- TRUE

# Prevent R CMD check from complaining about the use of pipe expressions
# standard data.table variables
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
      # data.table special symbols
      ".", ".I", ".N", ".SD",
      # column names used in data.table non-standard evaluation
      "begyear", "description", "endyear", "gdx", "iso3",
      "n", "pred", "reg", "refyear", "scenario", "tperiod",
      "V1", "V2", "value", "weight", "ww", "year",
      # package datasets referenced by name
      "default_weights", "region_descriptions", "region_mappings",
      "time_mappings", "witch_meta_param", "witch_period_year"
    ),
    utils::packageName()
  )
}
