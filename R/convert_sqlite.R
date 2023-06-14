#' Batch convert tables from a SQLite database
#'
#' \code{convert_sqlite} writes a converted SQLite database
#' in the \code{output_directory}.
#' All tables from the input \code{sqlitedb} are converted
#' using the \code{convert_table} function. Specific conversion options
#' are read in the parameter \code{meta_param} also stored in the
#' \code{sqlitedb}.
#'
#'
#' @family conversion functions
#' @seealso \code{\link{convert_table}}, \code{\link{convert_gdx}}.
#'
#' @param sqlitedb SQLITE file.
#' @param reg_id final regional aggregation.
#' @param time_id final time_period aggregation.
#' @param region_mappings a named list of region mapping data.table.
#' @param time_mappings a named list of time mapping data.table.
#' @param weights a named list of weights used by \code{convert_region}
#' @param output_directory directory where to write the converted SQLITE DB
#' @param region_name column name of the region, reg_id if null
#' @param guess_region input regional mapping if not explicitely defined
#' @param guess_input_t input time mapping if not explicitely defined
#' @param default_agg_missing default parameter to handle missing values in
#' \code{convert_region}
#' @param default_meta_param default meta_param
#' @export
#' @examples
#' \dontrun{
#' convert_sqlite("input/build/data_climate.sqlite", "witch17", "t30", "data_witch17")
#' }
#'
convert_sqlite <- function(sqlitedb,
                           reg_id,
                           time_id,
                           output_directory,
                           region_mappings = region_mappings,
                           time_mappings = time_mappings,
                           weights = default_weights,
                           guess_input_t = "t30",
                           region_name = NULL,
                           guess_region = "witch17",
                           default_agg_missing = "NA",
                           default_meta_param = NULL) {
  if (!file.exists(sqlitedb)) stop(paste(sqlitedb, "does not exist!"))
  if (is.null(region_name)) region_name <- reg_id

  cat(crayon::blue$bold(paste("Processing", basename(sqlitedb), "\n")))

  sqldb <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = sqlitedb)

  # tables collector
  tabs <- list()

  items <- RSQLite::dbListTables(sqldb)

  # load meta_data
  meta_param <- data.table::data.table(
    parameter = character(),
    type = character(),
    value = character()
  )
  meta_param <- rbind(meta_param, default_meta_param)
  if ("meta_param" %in% items) {
    query <- "select * from meta_param"
    dt_meta_param <- data.table::setDT(RSQLite::dbGetQuery(sqldb, query))
    new_param <- unique(dt_meta_param[["parameter"]])
    meta_param <- meta_param[!parameter %in% new_param]
    meta_param <- rbind(meta_param, dt_meta_param)
  }

  items <- items[items != "meta_param"]

  # Loop over all tables in the file
  for (item in items) {
    cat(crayon::blue(paste(" - table", item, "\n")))

    query <- paste0("select * from ", item)
    .data <- data.table::setDT(RSQLite::dbGetQuery(sqldb, query))

    item_param <- meta_param[parameter == item]

    res <- convert_item(
      .data,
      reg_id,
      time_id,
      region_name,
      item_param,
      time_mappings,
      region_mappings,
      weights,
      guess_region,
      guess_input_t,
      default_agg_missing
    )
    .data <- res[[1]]
    .info_share <- res[[2]]

    .i <- list(.data)
    names(.i) <- item

    tabs <- c(tabs, .i)

    # add an additionnal parameter '_info' when using sumby
    if (!is.null(.info_share)) {
      indices <- subset(colnames(.data), colnames(.data) != "value")
      data.table::setcolorder(.info_share, data_indices)
      names(.info_share) <- c(indices, "value")
      .i <- list(.info_share)
      names(.i) <- paste0(item, "_info")
      tabs <- c(tabs, .i)
    }
  }

  cat(crayon::blue(paste(" -", "writing SQLite db\n")))

  sqldb <- RSQLite::dbConnect(RSQLite::SQLite(),
    dbname = file.path(
      output_directory,
      basename(sqlitedb)
    )
  )
  for (i in seq_along(tabs)) {
    RSQLite::dbWriteTable(sqldb, names(tabs)[i], tabs[[i]],
      row.names = FALSE,
      overwrite = TRUE,
      append = FALSE,
      field.types = NULL
    )
  }

  RSQLite::dbDisconnect(sqldb)
}
