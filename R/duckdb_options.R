#' Set DuckDB optimization options
#'
#' Configure DuckDB performance settings for data conversion operations.
#' These options control memory usage, streaming behavior, and temporary storage.
#'
#' @param memory_limit Character string specifying memory limit (e.g., "4GB", "8GB").
#'   NULL (default) allows DuckDB to use automatic memory management.
#' @param streaming Logical. If TRUE, large tables are processed in chunks to reduce
#'   peak memory usage. Default is FALSE.
#' @param chunk_size Integer. Number of rows to fetch per chunk when streaming is enabled.
#'   Default is 100000. Larger values use more memory but may be faster.
#' @param temp_directory Character string. Directory path for DuckDB to store temporary
#'   files when data doesn't fit in memory. NULL (default) uses system temp directory.
#'
#' @details
#' These options affect how \code{\link{convert_duckdb}} processes database files:
#'
#' \strong{Memory Limit:} Constrains DuckDB's memory usage. Useful when processing
#' large datasets on systems with limited RAM. When memory limit is reached,
#' DuckDB will spill to disk (using temp_directory if specified).
#'
#' \strong{Streaming Mode:} When enabled, tables are fetched in chunks rather than
#' loading entirely into memory. This significantly reduces peak memory usage for
#' large tables, at a modest speed cost.
#'
#' \strong{Chunk Size:} Controls the trade-off between memory usage and performance
#' in streaming mode. Smaller chunks use less memory but require more round-trips
#' to the database.
#'
#' \strong{Temp Directory:} Specify a directory with sufficient space for DuckDB's
#' temporary files. Important when processing datasets larger than available RAM.
#'
#' @return Invisibly returns a list of the previous option values.
#'
#' @export
#' @examples
#' \dontrun{
#' # Set memory limit to 4GB and enable streaming for large tables
#' set_duckdb_options(memory_limit = "4GB", streaming = TRUE)
#'
#' # Use a specific temp directory with lots of space
#' set_duckdb_options(temp_directory = "/scratch/duckdb_temp")
#'
#' # Optimize for low memory systems
#' set_duckdb_options(
#'   memory_limit = "2GB",
#'   streaming = TRUE,
#'   chunk_size = 50000
#' )
#'
#' # Reset to defaults
#' set_duckdb_options(
#'   memory_limit = NULL,
#'   streaming = FALSE,
#'   chunk_size = 100000,
#'   temp_directory = NULL
#' )
#' }
set_duckdb_options <- function(memory_limit = NULL,
                                streaming = NULL,
                                chunk_size = NULL,
                                temp_directory = NULL) {
  # Store previous values
  old_values <- list(
    memory_limit = getOption("witchtools.duckdb.memory_limit"),
    streaming = getOption("witchtools.duckdb.streaming"),
    chunk_size = getOption("witchtools.duckdb.chunk_size"),
    temp_directory = getOption("witchtools.duckdb.temp_directory")
  )

  # Set new values (only if provided)
  if (!is.null(memory_limit)) {
    options(witchtools.duckdb.memory_limit = memory_limit)
  }
  if (!is.null(streaming)) {
    options(witchtools.duckdb.streaming = streaming)
  }
  if (!is.null(chunk_size)) {
    options(witchtools.duckdb.chunk_size = chunk_size)
  }
  if (!is.null(temp_directory)) {
    options(witchtools.duckdb.temp_directory = temp_directory)
  }

  invisible(old_values)
}


#' Get current DuckDB options
#'
#' Retrieve the current DuckDB optimization settings.
#'
#' @return A named list with current option values.
#' @export
#' @examples
#' \dontrun{
#' # View current settings
#' get_duckdb_options()
#' }
get_duckdb_options <- function() {
  list(
    memory_limit = getOption("witchtools.duckdb.memory_limit"),
    streaming = getOption("witchtools.duckdb.streaming"),
    chunk_size = getOption("witchtools.duckdb.chunk_size"),
    temp_directory = getOption("witchtools.duckdb.temp_directory")
  )
}
