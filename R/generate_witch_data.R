# Translate witch data
#
# This function translates WITCH data using a set of make_data files (R or gams).
#
# -o <output_dir>  Path to save the translated gdx (default: data_test)
# -n <region>      Region mapping, as in regions/*.inc (default: witch17)
# -t <time>        Time mapping (default: t30)
# -g <gdxfiles>    Specific gdx files/pattern to convert (default: input/build/data*)
# -d <data_dir> Path to the data folder (default: ../witch-data)'

translate_witch_data <- function(region = "witch17", timescale = "t30", output_dir = NULL, input_files = NULL, witch_data_dir) {

  # Region mapping
  data_reg_id = str_replace(as.character(region), ".inc", "")
  if (!file.exists(system.file("regions", paste0(data_reg_id, ".inc")), package = "witchdata")) {
    stop(paste("Region mapping", data_reg_id,"has no definition in the package."))
  }

  # Time mapping
  time_id = as.character(timescale)
  if (!file.exists(system.file("timescale", paste0(time_id, ".csv"), package = "witchdata"))) {
    stop(paste("Time mapping", time_id, "has no definition in the package."))
  }

  # Paths
  if (is.null(output_dir)) {
    output_directory = here(str_c("data_", data_reg_id,
                                  ifelse(time_id == "t30", "", str_c("_", time_id))))
  } else {
    output_directory = as.character(opts["o"])
  }
  if (!dir.exists(output_directory))
    dir.create(output_directory)

  ## Data directory
  if (is.null(opts[["d"]])) {
    data_directory = here("..","witch-data")
  } else {
    data_directory = as.character(opts["d"])
  }
  if (!dir.exists(data_directory)){
    stop(paste0("data directory '", data_directory, "' does not exist!\n go to https://github.com/witch-team/witch-data and follow instructions"), call. = FALSE)
  }


}
