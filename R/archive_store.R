#' Try to save the archive in a list of directories.
#'
#' @param filename the name of the archive to save.
#' @param dir_list a list of potential directories where to save the archive.
#'
#' @returns Invisibly \code{NULL}. Called for its side effect of copying
#' \code{filename} into each existing directory of \code{dir_list}.
#'
#' @export
#'
archive_store <- function(filename, dir_list) {

  .Deprecated()

  # Copy file in the archive
  cp_file <- function(folder, .f) {
    if (dir.exists(folder)) {
      # The destination is folder/<basename>, not folder/<full source path>;
      # check and clear that so the copy overwrites on every platform (the
      # Linux "cp" branch overwrites on its own, fs::file_copy() does not).
      dest <- fs::path(folder, basename(.f))
      if (fs::file_exists(dest)) {
        fs::file_delete(dest)
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
