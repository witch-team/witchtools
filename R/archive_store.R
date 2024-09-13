#' Try to save the archive in a list of directories.
#'
#' @param filename the name of the archive to save.
#' @param dir_list a list of potential directories where to save the archive.
#' @export
#'
archive_store <- function(filename, dir_list) {

  .Deprecated()

  # Copy file in the archive
  cp_file <- function(folder, .f) {
    if (dir.exists(folder)) {
      if (fs::file_exists(fs::path(folder,.f))) {
        fs::file_delete(fs::path(folder,.f))
      }
      if (Sys.info()["sysname"]=="Linux" & !stringr::str_detect(folder, " ")) {
        system(paste("cp", .f, folder))
      } else {
        fs::file_copy(.f, folder)
      }
      cat(paste("Copied", .f, "to", folder, "\n"))
    }
  }
  lapply(dir_list, cp_file, .f = filename)

  return(invisible(NULL))

}
