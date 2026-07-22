#' Convert a data.table from a regional mapping to another.
#'
#' \code{convert_region} returns a list containing a data.table where values are
#' converted from one regional mapping to another. The conversion is done by
#' downscaling values at country-level (if necessary) and updscaling them
#' into the final regional mapping. The list also contain an
#' optional data.table about the value coperture for the operator \code{sumby}.
#' The required format of the input data is described in Details.
#'
#' The input data.table should contain a column "value" and a column named by
#' the initial regional mapping \code{from_reg},
#' while the other columns are are considered as id columns.
#' The resulting data.table have a column named by the final regional
#' mapping \code{to_reg}.
#'
#' The regional mapping for \code{from_reg} and \code{to_reg} can be
#' provided in a named list through the parameter \code{regions}.
#' Regional mappings are 2-columns data.table with a column named 'iso3'
#' (for country ISO3)
#' and another one named as the regional mapping (for region name).
#' The name in the list should also be the regional mapping name.
#'
#' Region-to-region conversions use a fast engine that converts through a
#' precomputed region-pair coefficient table instead of expanding the data to
#' country level, which drastically reduces memory use and run time on large
#' tables. Results are identical up to floating-point summation order
#' (relative differences below 1e-12). The previous implementation remains
#' available with \code{options(witchtools.convert_region_engine = "legacy")}.
#' Country-level (iso3) input and the \code{set1} operator always use the
#' legacy engine.
#'
#' @family conversion functions
#' @seealso \code{\link{convert_table}},
#' \code{\link{convert_gdx}}.
#'
#'
#' @param .x a well-formatted data.table.
#' @param from_reg initial regional mapping name or a data.table with
#' the mapping.
#' @param to_reg final regional mapping name  or a data.table with the mapping.
#' @param agg_operator aggregation operator (See Details for the list of
#' possible values)
#' @param agg_weight aggregation weight data.table (See Details for the list of
#' possible values)
#' @param agg_missing tells how to deal with missing values ("NA" or "zero")
#' @param regions optional list of region mappings (see Details for format)
#' @param info logical indicating whether to include information, only required
#'  for the agg_operator "sumby".
#'
#' @return a list containing a converted data.table and information about
#'         the coperture if available.
#' @export
#' @examples
#' # Aggregate country-level GDP into the 17 WITCH regions
#' gdp_iso3 <- data.table::copy(default_weights[["gdp"]])
#' data.table::setnames(gdp_iso3, "weight", "value")
#'
#' convert_region(gdp_iso3, to_reg = "witch17")
#'
convert_region <- function(.x,
                           from_reg = NULL,
                           to_reg,
                           agg_operator = "sum",
                           agg_weight = witchtools::default_weights[["gdp"]],
                           agg_missing = "NA",
                           regions = witchtools::region_mappings,
                           info = FALSE) {

  error <- iso3 <- weight <- NULL # due to NSE notes in R CMD check
  sum_weight <- value <- NULL # due to NSE notes in R CMD check

  if (!data.table::is.data.table(.x)) .x <- data.table::setDT(.x)

  # Check if the value column exists
  if (!"value" %in% colnames(.x)) {
    stop("The input data.table should contain a column named 'value'.")
  }

  # Check regions is a list of data.table
  all_are_dts <- all(sapply(regions, data.table::is.data.table))
  if (!all_are_dts) {
    stop("regions must be a list of data.table.")
  }

  # Check weight
  if (is.null(agg_weight)) {
    stop("agg_weight is NULL. Check if it is well-defined.")
  }

  # Guess initial region from column names
  if (is.null(from_reg)) {
    guess_reg <- intersect(colnames(.x), unique(c("iso3", names(regions))))
    if (length(guess_reg) == 1) {
      from_reg <- guess_reg
    }
  }

  # Initial mapping
  if (is.character(from_reg)) {
    if (!from_reg %in% c("iso3", names(regions))) {
      stop(paste0("regions should contains the name ", from_reg, "."))
    }
    rmap0 <- regions[[from_reg]]
    rname0 <- from_reg
  } else if (data.table::is.data.table(from_reg)) {
    rmap0 <- from_reg
    rname0 <- region_id(rmap0)
  } else {
    stop(paste0("from_reg should be a character or a data.table."))
  }

  # Final mapping
  if (is.character(to_reg)) {
    if (!to_reg %in% c("iso3", names(regions))) {
      stop(paste0("regions should contains the name ", to_reg, "."))
    }
    rmap1 <- regions[[to_reg]]
    rname1 <- to_reg
  } else if (data.table::is.data.table(to_reg)) {
    rmap1 <- to_reg
    rname1 <- region_id(rmap1)
  } else {
    stop(paste0("to_reg should be a character or a data.table."))
  }

  # Same mappings input-output, do nothing
  if (rname0 == rname1) {
    return(.x)
  }

  # "sumby" might need info
  if (agg_operator == "sumby" & !info) {
    warning(paste0("Operator sumby might need info = TRUE."))
  }

  # Not yet implemented
  if (rname1 == "iso3") {
    stop(paste0("to_reg == iso3 is not yet implemented."))
  }

  # Engine dispatch. The "fast" engine converts region->region input through
  # a small pair-coefficient table instead of the iso3 explosion; iso3-level
  # input is already linear-size and stays on the legacy engine. "set1" also
  # stays on the legacy engine: its round() is discontinuous, so the summation
  # reassociation of the fast engine could flip a value sitting exactly on a
  # .5 boundary, and set1 tables are tiny anyway.
  engine <- getOption("witchtools.convert_region_engine", "fast")
  if (identical(engine, "fast") && rname0 != "iso3" && agg_operator != "set1") {
    return(convert_region_fast(.x, rmap0, rname0, rmap1, rname1,
                               agg_operator, agg_weight, agg_missing, info))
  }
  convert_region_via_iso3(.x, rmap0, rname0, rmap1, rname1,
                          agg_operator, agg_weight, agg_missing, info)
}
