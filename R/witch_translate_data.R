#' Translate WITCH data (run make_data files and convert data).
#'
#' @param witch_dir WITCH main directory
#' @param region final regional aggregation
#' @param timescale final timescale aggregation
#' @param idir input data folder (for weights and to be pass to make_data files)
#' @param output_dir output folder (to overidde default WITCH data folder name)
#'
#' @export
#' @examples
#' \dontrun{
#' witch_translate_data(region = "r5")
#' witch_translate_data(region = "r5", output_dir = 'data_r5')
#' }
witch_translate_data <- function(witch_dir = ".",
                                 region,
                                 timescale,
                                 idir = NULL,
                                 output_dir = NULL,
                                 regions = region_mappings,
                                 times = time_mappings) {

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

  cat(crayon::silver$bold("\U26AB Process input gdx(s)\n"))

  # TODO move outside this function
  find_modified_gdx <- function(input_directory,
                               output_directory,
                               force = FALSE) {
    input_gdx <- Sys.glob(file.path(input_directory, "data_*.gdx"))
    if (force)
      return(input_gdx)
    output_gdx <- file.path(output_directory, basename(input_gdx))
    todo <- file.mtime(input_gdx) > file.mtime(output_gdx)
    return(input_gdx[is.na(todo) | todo])
  }

  gdxlist <- sort(find_modified_gdx(input_directory, output_directory,
                                    force = FALSE))

  # convert all gdx
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

  # Translate GLOBIOM dataset
  input_gb <- file.path(input_directory,'data_globiom.sqlite')
  output_gb <- file.path(output_directory,'data_globiom.sqlite')
  todo <- FALSE
  todo <- !file.exists(output_gb)
  if (!todo) {
    todo <- file.mtime(input_gb) > file.mtime(output_gb)
  }
  if (todo) {
    res <- convert_globiom(input_gb,
                           reg_id,
                           time_id,
                           region_mappings = regions,
                           time_mappings = times,
                           weights = default_weights,
                           output_directory)
  }

  cat(crayon::silver$bold(paste("\U26AB Create gams files for WITCH\n")))

  write_gams(regions[[reg_id]],
             times[[time_id]],
             output_directory)

}
