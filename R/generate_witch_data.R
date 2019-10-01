#' Translate WITCH data (run make_data files and convert data).
#'
#' @param witch_dir WITCH main directory
#' @param region final regional aggregation
#' @param timescale final timescale aggregation
#' @param data_dir witch-data folder (weights and to be pass to make_data files)
#' @param output_dir output folder (to overidde default WITCH data folder name)
#'
#' @export
#' @examples
#' \dontrun{
#' witch_translate_data(region = "r5")
#' witch_translate_data(region = "r5", data_dir = 'witch-data', output_dir = 'data_r5')
#' }
witch_translate_data <- function(witch_dir = ".", region = "witch17", timescale = "t30", data_dir = NULL, output_dir = NULL) {

  cat(crayon::silver$bold("\U26AB Initialisation\n"))

  # Check if gdxtool is available and working
  if (requireNamespace('gdxtools', quietly = TRUE)) {
    gdxtools::igdx(dirname(Sys.which('gams'))) # Please have gams in your PATH!
  }

  # Region mapping
  reg_id = stringr::str_replace(as.character(region), ".inc", "")
  if (!file.exists(system.file("regions", paste0(reg_id, ".inc"), package = "witchtools"))) {
    stop(paste("Region mapping", reg_id,"has no definition in the package."))
  }

  # Time mapping
  time_id = as.character(timescale)
  if (!file.exists(system.file("timescale", paste0(time_id, ".csv"), package = "witchtools"))) {
    stop(paste("Time mapping", time_id, "has no definition in the package."))
  }

  # Paths
  if (is.null(output_dir)) {
    output_directory = file.path(witch_dir, stringr::str_c("data_", reg_id,
                                  ifelse(time_id == "t30", "", stringr::str_c("_", time_id))))
  } else {
    output_directory = as.character(output_dir)
  }
  output_directory <- normalizePath(output_directory)
  if (!dir.exists(output_directory))
    dir.create(output_directory)

  ## Data directory
  if (is.null(data_dir)) {
    data_directory = normalizePath(file.path(witch_dir,"..","witch-data"))
  } else {
    data_directory = normalizePath(as.character(data_dir))
  }
  if (!dir.exists(data_directory)) {
    warning(paste0("data directory '", data_directory, "' does not exist; this might causes some problems."))
  }

  cat(crayon::silver$bold("\U26AB Regional and timescale mappings\n"))

  cat(crayon::blue(paste("  - Output timescale:", time_id, "\n")))
  cat(crayon::blue(paste("  - Output region:", reg_id, "\n")))
  cat(crayon::blue(paste("  - Output directory:", output_directory, "\n")))

  # Region mappings
  region_mapping_files = Sys.glob(file.path(system.file("regions", package = "witchtools"),"*.inc"))
  region_mappings <- lapply(region_mapping_files, load_region_mapping)
  region_definitions <- lapply(region_mapping_files, load_region_definition)
  names(region_mappings) <- names(region_definitions) <- stringr::str_sub(basename(region_mapping_files), 1, -5)

  # Timescale mappings
  time_mapping_files = Sys.glob(file.path(system.file("timescale", package = "witchtools"), "*.csv"))
  time_mappings = lapply(time_mapping_files, load_timescale_mapping)
  names(time_mappings) = stringr::str_sub(basename(time_mapping_files), 1, -5)

  cat(crayon::silver$bold("\U26AB Run make_data files\n"))

  input_directory = file.path(witch_dir,"input","build")
  if (!dir.exists(input_directory)) dir.create(input_directory)

  # GAMS files make_data_*.gms
  gamsfiles = Sys.glob(file.path(witch_dir,'input','make_data_*.gms'))
  gamsfiles = gamsfiles[gamsfiles != "make_data_template.gms"]
  res <- for (gamsfile in gamsfiles) {
    make_data_gms(gamsfile, data_directory, witch_dir)
  }

  # Rscript files make_data_*.gms
  Rfiles = Sys.glob(file.path(witch_dir,'input','make_data_*.R'))
  res <- for (Rfile in Rfiles) {
    make_data_R(Rfile, data_directory, witch_dir)
  }

  cat(crayon::silver$bold("\U26AB Process input gdx(s)\n"))

  # TODO move outside this function
  find_modified_gdx = function(input_directory,
                               output_directory,
                               force = F) {
    input_gdx = Sys.glob(file.path(input_directory, "data_*.gdx"))
    if (force)
      return(input_gdx)
    output_gdx = file.path(output_directory, basename(input_gdx))
    todo = file.mtime(input_gdx) > file.mtime(output_gdx)
    return(input_gdx[is.na(todo) | todo])
  }

  gdxlist = sort(find_modified_gdx(input_directory, output_directory, force = F))

  weights = load_weights(data_directory, region_mappings)

  # convert all gdx
  if (requireNamespace('gdxtools', quietly = TRUE)) {
    for (gdxfile in gdxlist) {
      convert_gdx(
        gdxfile,
        reg_id,
        time_id,
        region_mappings,
        region_definitions,
        time_mappings,
        weights,
        output_directory
      )
    }
  }

  # Translate GLOBIOM dataset
  input_gb = file.path(input_directory,'data_globiom.sqlite')
  output_gb = file.path(output_directory,'data_globiom.sqlite')
  todo = F
  todo = !file.exists(output_gb)
  if (!todo) {
    todo = file.mtime(input_gb) > file.mtime(output_gb)
  }
  if (todo) {
    res <- convert_globiom(input_gb,
                           reg_id,
                           time_id,
                           region_mappings,
                           region_definitions,
                           time_mappings,
                           weights,
                           output_directory)
  }

  cat(crayon::silver$bold(paste("\U26AB Create additional gams files for WITCH\n")))

  write_gams(reg_id,
             time_id,
             region_mappings,
             region_definitions,
             time_mappings,
             weights,
             output_directory)

}
