#' Batch convert parameters and variables from a GDX
#'
#' \code{convert_gdx} writes a converted GDX in the \code{output_directory}.
#' All parameters and variables from the input \code{gdxfile} are converted
#' using the \code{convert_table} function. Specific conversion options
#' are read in the parameter \code{meta_param} also stored in the gdxfile.
#'
#'
#' @family conversion functions
#' @seealso \code{\link{convert_table}}, \code{\link{convert_sqlite}}.
#'
#' @param gdxfile location of the GDX file.
#' @param reg_id final regional aggregation.
#' @param time_id final time_period aggregation.
#' @param region_mappings a named list of region mapping data.table.
#' @param time_mappings a named list of time mapping data.table.
#' @param weights a named list of weights used by \code{convert_region}
#' @param output_directory directory where to write the converted GDX
#' @param region_name column name of the region, reg_id if null
#' @param guess_region input regional mapping if not explicitely defined
#' @param guess_input_t input time mapping if not explicitely defined
#' @param default_agg_missing default parameter to handle missing values in
#' \code{convert_region}
#' @param default_meta_param default meta_param
#' @export
#' @examples
#' \dontrun{
#' convert_gdx("input/build/data_climate.gdx", "witch17", "t30", "data_witch17")
#' }
#'
convert_gdx <- function(gdxfile,
                        reg_id,
                        time_id,
                        output_directory,
                        region_mappings = region_mappings,
                        time_mappings = time_mappings,
                        weights = witchtools::default_weights,
                        guess_input_t = "t30",
                        region_name = NULL,
                        guess_region = "witch17",
                        default_agg_missing = "zero",
                        default_meta_param = NULL) {

  parameter <- year <- type <- value <- NULL # due to NSE notes in R CMD check

  if (!file.exists(gdxfile)) stop(paste(gdxfile, "does not exist!"))
  if (is.null(region_name)) region_name <- reg_id

  cat(crayon::blue$bold(paste("Processing", basename(gdxfile), "\n")))

  .gdx <- gdxtools::gdx(gdxfile)

  # parameter collector
  params <- list()
  vars <- list()

  # load meta_data
  meta_param <- data.table::data.table(
    parameter = character(),
    type = character(),
    value = character()
  )
  meta_param <- rbind(meta_param, default_meta_param)
  if ("meta_param" %in% .gdx$sets$name) {
    dt_meta_param <- data.table::setDT(.gdx["meta_param"])
    names(dt_meta_param) <- c("parameter", "type", "value")
    new_param <- unique(dt_meta_param[["parameter"]])
    meta_param <- meta_param[!parameter %in% new_param]
    meta_param <- rbind(meta_param, dt_meta_param)
  }

  items <- c(.gdx$parameters$name, .gdx$variables$name)

  # Loop over all parameters and variables in the file
  for (item in items) {
    item_type <- ifelse(item %in% .gdx$parameters$name, "parameter", "variable")

    item_param <- meta_param[parameter == item]

    cat(crayon::blue(paste(" -", item_type, item, "\n")))

    # uses as.data.table to keep attribute 'gams'
    .data <- data.table::setDT(.gdx[item])
    text <- attributes(.data)[["gams"]]

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

    # Table indices are stars, except from t and n
    if (length(colnames(.data)) == 1) {
      names(.data) <- "value"
    } else {
      indices <- subset(colnames(.data), colnames(.data) != "value")
      indices <- ifelse(indices %in% c(region_name, "t"), indices, "*")
      names(.data) <- c(indices, "value")
    }

    # add to collector []
    attributes(.data) <- c(attributes(.data), gams = text)

    # add warnings if data contains NAs
    if (nrow(.data) > 0 & !item %in% c("carbonprice")) {
      if (anyNA(.data$value)) {
        warning(paste(gdxfile, "-", item, "contains NAs."))
      }
    }

    .i <- list(.data)
    names(.i) <- item

    if (item_type == "parameter") params <- c(params, .i)
    if (item_type == "variable") vars <- c(vars, .i)

    # add an additionnal parameter '_info' when using sumby
    if (!is.null(.info_share)) {
      names(.info_share) <- c(indices, "value")
      .i <- list(.info_share)
      names(.i) <- paste0(item, "_info")
      params <- c(params, .i)
    }
  }

  cat(crayon::blue(paste(" -", "writing gdx\n")))

  f <- file.path(output_directory, basename(gdxfile))

  gdxtools::write.gdx(f, params = params, vars_l = vars)

  return(gdxfile)
}
