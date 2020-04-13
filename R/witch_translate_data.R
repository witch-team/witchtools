#' Translate WITCH data (run make_data files and convert data).
#'
#' \code{witch_translate_data} generates the input data for the WITCH model.
#' First, it will run the R `make_data_files` and the gams `make_data_files`
#' usually located in the input folder of WITCH. These make_data_files produce
#' gdx and sqlite files in the `build` folder. Time-series should be yearly.
#' Spatial data should be preferably described at ISO3 level, but the script
#' will handle any regional-mapping contained in \code{regions}.
#'
#' The regional mappings should be
#' provided in a named list through the parameter \code{regions}.
#' Regional mappings are 2-columns data.table with a column named 'iso3'
#' (for country ISO3)
#' and another one named as the regional mapping (for region name).
#' The name in the list should also be the regional mapping name.
#'
#' \code{times} is a list of time mapping between year and time period which
#' should be provided as a data.table with columns "year" and "t",
#' and refyear for interpolation and extrapolation. \code{times} should contain
#' \code{timescale}
#'
#' @param witch_dir WITCH main directory
#' @param region final regional aggregation
#' @param timescale final timescale aggregation
#' @param idir input data folder (for weights and to be pass to make_data files)
#' @param output_dir output folder (to overidde default WITCH data folder name)
#' @param regions optional list of regional mappings (see Details for format)
#' @param times optional list of timescale mappings (see Details for format)
#' @param force logical indicating whether all make files should be processed
#'
#'
#' @export
#' @examples
#' \dontrun{
#' witch_translate_data(region = "r5", timescale = 't30')
#' }
witch_translate_data <- function(witch_dir = ".",
                                 region,
                                 timescale,
                                 idir = NULL,
                                 output_dir = NULL,
                                 regions = region_mappings,
                                 times = time_mappings,
                                 force = FALSE) {

  cat(crayon::silver$bold("\U26AB Initialisation\n"))

  # Check if gdxtool is available and working
  if (requireNamespace('gdxtools', quietly = TRUE)) {
    gdxtools::igdx(dirname(Sys.which('gams'))) # Please have gams in your PATH!
  }

  # Region mapping
  reg_id <- stringr::str_replace(as.character(region), ".inc", "")

  # Time mapping
  time_id <- as.character(timescale)

  # Paths
  if (is.null(output_dir)) {
    output_directory <- file.path(witch_dir, stringr::str_c("data_", reg_id,
                                  ifelse(time_id == "t30", "",
                                         stringr::str_c("_", time_id))))
  } else {
    output_directory <- as.character(output_dir)
  }
  output_directory <- normalizePath(output_directory)
  if (!dir.exists(output_directory))
    dir.create(output_directory)

  ## Data directory
  if (is.null(idir)) {
    idir <- normalizePath(file.path(witch_dir,"input","data"))
  } else {
    idir <- normalizePath(as.character(idir))
  }
  stopifnot(dir.exists(idir))

  cat(crayon::silver$bold("\U26AB Regional and timescale mappings\n"))

  cat(crayon::blue(paste("  - Output timescale:", time_id, "\n")))
  cat(crayon::blue(paste("  - Output region:", reg_id, "\n")))
  cat(crayon::blue(paste("  - Output directory:", output_directory, "\n")))

  cat(crayon::silver$bold("\U26AB Run make_data files\n"))

  input_directory <- file.path(witch_dir,"input","build")
  if (!dir.exists(input_directory)) dir.create(input_directory)

  # Rscript files make_data_*.R [FIRST]
  Rfiles <- Sys.glob(file.path(witch_dir,'input','make_data_*.R'))
  res <- for (Rfile in Rfiles) {
    make_data_R(Rfile, idir, witch_dir)
  }

  # GAMS files make_data_*.gms
  gamsfiles <- Sys.glob(file.path(witch_dir,'input','make_data_*.gms'))
  gamsfiles <- gamsfiles[gamsfiles != "make_data_template.gms"]
  res <- for (gamsfile in gamsfiles) {
    make_data_gms(gamsfile, idir, witch_dir)
  }

  cat(crayon::silver$bold("\U26AB Process input gdx file(s)\n"))

  # Find gdx files to be processed
  gdxlist <- Sys.glob(file.path(input_directory, "data_*.gdx"))
  if (!force) {
    output_gdx <- file.path(output_directory, basename(gdxlist))
    todo <- file.mtime(gdxlist) > file.mtime(output_gdx)
    gdxlist <- gdxlist[is.na(todo) | todo]
  }
  gdxlist <- sort(gdxlist)

  if (requireNamespace('gdxtools', quietly = TRUE)) {
    for (gdxfile in gdxlist) {
      convert_gdx(
        gdxfile,
        reg_id,
        time_id,
        region_mappings = regions,
        time_mappings = times,
        weights = default_weights,
        region_name = "n",
        output_directory,
        default_meta_param = witch_meta_param
      )
    }
  }

  cat(crayon::silver$bold("\U26AB Process input SQLite file(s)\n"))

  # Find sqlite files to be processed
  sqllist <- Sys.glob(file.path(input_directory, "data_*.sqlite"))
  if (!force) {
    output_sqlite <- file.path(output_directory, basename(sqllist))
    todo <- file.mtime(sqllist) > file.mtime(output_sqlite)
    sqllist <- sqllist[is.na(todo) | todo]
  }
  sqllist <- sort(sqllist)

  if (requireNamespace('RSQLite', quietly = TRUE)) {
    for (sqlfile in sqllist) {
      convert_sqlite(
        sqlfile,
        reg_id,
        time_id,
        region_mappings = regions,
        time_mappings = times,
        weights = default_weights,
        region_name = "n",
        output_directory,
        default_meta_param = witch_meta_param
      )
    }
  }

  cat(crayon::silver$bold(paste("\U26AB Create gams files for WITCH\n")))

  witch_write_gams(regions[[reg_id]], times[[time_id]], output_directory)

}
