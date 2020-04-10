#' Batch convert tables from a SQLite database
#'
#' \code{convert_gdx} writes a converted SQLite database
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
#' convert_sqlite('input/build/data_climate.sqlite','witch17','t30','data_witch17')
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
                           default_meta_param = NULL){

  if (!file.exists(sqlitedb)) stop(paste(sqlitedb, "does not exist!"))
  if (is.null(region_name)) region_name <- reg_id

  cat(crayon::blue$bold(paste("Processing", basename(sqlitedb), "\n")))

  sqldb <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = sqlitedb)

  # tables collector
  tabs <- list()

  items <- RSQLite::dbListTables(sqldb)

  # load meta_data
  meta_param <- data.table::data.table(parameter = character(),
                                       type = character(),
                                       value = character())
  meta_param <- rbind(meta_param,default_meta_param)
  if ("meta_param" %in% items) {
    query <- 'select * from meta_param'
    dt_meta_param <- data.table::setDT(RSQLite::dbGetQuery(sqldb, query))
    new_param <- unique(dt_meta_param[['parameter']])
    meta_param <- meta_param[!parameter %in% new_param]
    meta_param <- rbind(meta_param,dt_meta_param)
  }

  items <- items[items != "meta_param"]

  #Loop over all tables in the file
  for (item in items) {

    cat(crayon::blue(paste(" - table",item,"\n")))

    item_param <- meta_param[parameter == item]

    query <- paste0('select * from ',item)
    .data <- data.table::setDT(RSQLite::dbGetQuery(sqldb, query))

    if (region_name %in% colnames(.data)) {
      data.table::setnames(.data, region_name, guess_region)
    }
    if ("t" %in% colnames(.data) & guess_input_t == "t30") {
      data.table::setnames(.data, "t", "year")
      .data[,year := paste(as.numeric(.data$year) * 5 + 2000)]
    }

    data_indices <- colnames(.data)

    convpar <- list()

    # Time period conversion
    do_time_period <- ("year" %in% colnames(.data))

    if (do_time_period) {
      data_indices[data_indices == "year"] <- "t"
    }

    # Region conversion
    data_reg <- intersect(colnames(.data), names(region_mappings))
    do_region <- FALSE
    # deal with case when data_reg == character(0)
    if (length(data_reg) == 1) {
      do_region <- (data_reg != reg_id)
      data_indices[data_indices == data_reg] <- region_name
    }
    from_reg <- NULL
    to_reg <- NULL

    if (do_region) {

      # Ensure region is lower case / iso3 is upper case
      if (data_reg != "iso3") {
        .idx <- which(data_reg == colnames(.data))
        .data[[.idx]] <- tolower(.data[,get(data_reg)])
      } else {
        .data$iso3 <- toupper(.data$iso3)
      }

      # Set initial regional mapping
      if (data_reg != "iso3") {
        from_reg <- region_mappings[[data_reg]]
      } else {
        from_reg <- 'iso3'
      }

      # Set final regional mapping
      to_reg <- region_mappings[[reg_id]]

    }

    # Conversion options
    convopt <- c(convpar,
                 do_extrap = (nrow(item_param[type == "extrap" &
                                                value == "skip"]) == 0),
                 do_interp = (nrow(item_param[type == "interp" &
                                                value == "skip"]) == 0),
                 do_past_extrap = TRUE)

    if (!convopt[['do_extrap']]) {
      convopt[['do_past_extrap']] <- FALSE
    }

    # No inter/extrapolation for stochastic branch
    if (stringr::str_detect(time_id, "branch")) {
      convopt[['do_extrap']] <- FALSE
      convopt[['do_interp']] <- FALSE
    }

    # Missing values
    convopt[['agg_missing']] <- default_agg_missing
    if (nrow(item_param[type == "missing_values"]) > 0) {
      convopt[['agg_missing']] <- item_param[type == "missing_values"][1,value]
    }

    # Aggregation operator
    convopt[['agg_operator']] <- "sum"
    if (nrow(item_param[type == "nagg"]) > 0) {
      convopt[['agg_operator']] <- item_param[type == "nagg"][1,value]
    }

    # Aggregation weight
    nweight <- "pop"
    if (nrow(item_param[type == "nweight"]) > 0) {
      nweight <- item_param[type == "nweight"][1,value]
    } else {
      nweight <- ifelse(convopt[['agg_operator']] %in% c("mean"),"cst","gdp")
    }
    # Ensure max and min have cst weight
    if (convopt[['agg_operator']] %in% c("min","max")) {
      nweight <- "cst"
    }

    if (!nweight %in% names(weights)) {
      stop(paste(nweight,'not in weights!'))
    }

    .conv <- convert_table(.data,
                           time_mapping = time_mappings[[time_id]],
                           from_reg = from_reg,
                           to_reg = region_mappings[[reg_id]],
                           agg_weight = weights[[nweight]],
                           options = convopt,
                           do_time_period = do_time_period,
                           do_region = do_region,
                           verbose = TRUE,
                           info = TRUE)

    .data <- .conv[['data']]
    .info_share <- .conv[['info']]

    if (reg_id %in% colnames(.data)) {
      data.table::setnames(.data, reg_id, region_name)
    }

    # rename reg_id in necessary (when no region conversion is required)
    if (reg_id %in% colnames(.data)) {
      cat(crayon::blue(paste("   [same]\n")))
    } else if (do_region) {
      cat(crayon::blue(paste0("   [agg: ", convopt[['agg_operator']], ", wgt: ",
                              nweight, "]\n")))
    }

    # Ensure the original order is kept
    data.table::setcolorder(.data,data_indices)

    .i <- list(.data)
    names(.i) <- item

    tabs  <- c(tabs, .i)

    # add an additionnal parameter '_info' when using sumby
    if (nweight %in% c("sumby")) {
      data.table::setcolorder(.info_share,data_indices)
      names(.info_share) <- c(indices,"value")
      .i <- list(.info_share)
      names(.i) <- paste0(item,'_info')
      tabs <- c(tabs, .i)
    }

  }

  cat(crayon::blue(paste(" -","writing SQLite db\n")))

  sqldb <- RSQLite::dbConnect(RSQLite::SQLite(),
                              dbname = file.path(output_directory,
                                                 basename(sqlitedb)))
  for (i in seq_along(tabs)) {
    RSQLite::dbWriteTable(sqldb, names(tabs)[i], tabs[[i]],
                          row.names = FALSE,
                          overwrite = TRUE,
                          append = FALSE,
                          field.types = NULL)
  }

  RSQLite::dbDisconnect(sqldb)

}

