#' Try to save the archive in a list of directories.
#'
#' @param filename the name of the archive to save.
#' @param dir_list a list of potential directories where to save the archive.
#' @param extract_dir the directory where to unzip the archive
#' @export
#'
archive_restore <- function(filename, dir_list, extract_dir = ".") {

  .Deprecated()


  # Find if the file exists in folders
  arch_path <- fs::path(dir_list, filename)
  arch_path <- arch_path[fs::file_exists(arch_path)]

  # If the file exists, unzip it into the output directory
  if (length(arch_path) > 0) {
    cat(paste("Found archived data in:", arch_path[1], "\n"))
    zip::unzip(arch_path[1], exdir = extract_dir)
    return(arch_path[1])
  }

  return(invisible(NULL))

}
