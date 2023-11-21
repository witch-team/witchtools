#' Return a name for a archive name, based on time-region mappings and git commit
#' information.
#'
#' @param reg_id a character for the region id.
#' @param time_id a character for the time id.
#' @param ext the extension of the archive file (default:".zip").
#' @export
#'
archive_filename <- function(reg_id, time_id, ext = ".zip") {

  # Get Commit ID
  commit_id <- system("git rev-parse HEAD", intern = TRUE)

  # Keep the 6 first digit of the commit
  commit_id <- stringr::str_sub(commit_id, 1, 6)

  # Get Commit Date in short format
  commit_date_raw <- system("git show -s --format=%ci HEAD", intern = TRUE)

  # Convert to date-time object
  commit_date <- strptime(commit_date_raw, format = "%Y-%m-%d %H:%M:%S %z")

  # Convert to short format
  commit_date <- format(commit_date, format = "%Y%m%d-%H%M")

  # Create a filname with the commit ID and the date
  filename <- stringr::str_c("data_", reg_id, "_", time_id, "_", commit_id, "_",
                    commit_date, ext)

  return(filename)
}
