# Translate witch data
#' @export
translate_witch_data <- function(witch_dir = ".", region = "witch17", timescale = "t30", data_dir = NULL, output_dir = NULL, input_files = NULL) {

  cat(crayon::silver$bold("● Initialisation\n"))

  # Check if gdxtool is available and working
  if ("gdxtools" %in% rownames(installed.packages())) {
    gdxtools::igdx(dirname(Sys.which('gams'))) # Please have gams in your PATH!
  }

  # Region mapping
  reg_id = stringr::str_replace(as.character(region), ".inc", "")
  if (!file.exists(system.file("regions", paste0(reg_id, ".inc"), package = "witchdata"))) {
    stop(paste("Region mapping", reg_id,"has no definition in the package."))
  }

  # Time mapping
  time_id = as.character(timescale)
  if (!file.exists(system.file("timescale", paste0(time_id, ".csv"), package = "witchdata"))) {
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

  cat(crayon::silver$bold("● Regional and timescale mappings\n"))

  cat(crayon::blue(paste("  - Output timescale:", time_id, "\n")))
  cat(crayon::blue(paste("  - Output region:", reg_id, "\n")))
  cat(crayon::blue(paste("  - Output directory:", output_directory, "\n")))

  # Region mappings
  region_mapping_files = Sys.glob(file.path(system.file("regions", package = "witchdata"),"*.inc"))
  region_mappings <- lapply(region_mapping_files, load_region_mapping)
  region_definitions <- lapply(region_mapping_files, load_region_definition)
  names(region_mappings) <- names(region_definitions) <- stringr::str_sub(basename(region_mapping_files), 1, -5)

  # Timescale mappings
  time_mapping_files = Sys.glob(file.path(system.file("timescale", package = "witchdata"), "*.csv"))
  time_mappings = lapply(time_mapping_files, load_timescale_mapping)
  names(time_mappings) = stringr::str_sub(basename(time_mapping_files), 1, -5)

  if (is.null(input_files)) {

    cat(crayon::silver$bold("● Run make_data files\n"))

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

  } else {

    writeLines("● Run make_data files (skipped)")

  }

  cat(crayon::silver$bold("● Process input gdx(s)\n"))

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

  if (is.null(input_files)) {
    gdxlist = sort(find_modified_gdx(input_directory, output_directory, force = F))
  } else {
    gdxlist = Sys.glob(input_files)
  }

  weights = load_weights(data_directory, region_mappings)

  # convert all gdx
  if ("gdxtools" %in% rownames(installed.packages())) {
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

  cat(crayon::silver$bold(paste("● Create additional gams files for WITCH\n")))

  write_gams(reg_id,
             time_id,
             region_mappings,
             region_definitions,
             time_mappings,
             weights,
             output_directory)


}
