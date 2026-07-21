#' Fast query of WITCH results files
#'
#' Returns a formatted data.table from a list of results files.
#' It adds a scenario column
#'
#' @family WITCH helper functions
#'
#' @param item parameter or variable name
#' @param resgdx list of WITCH results gdx
#' @param filter named list of filter (eg. list(e="CO2",n="brazil,usa")).
#'               if n contains "world", then the sum of n is computed.
#' @param scenarios vector of scenario names in same order than resgdx
#' @param keep_gdx keep gdx file name in the result
#' @param keep_t keep t in the result
#' @param valigdx optional gdx file with validation data. Reserved for future
#' use; currently ignored.
#' @param histgdx optional gdx file with historical data. Reserved for future
#' use; currently ignored.
#' @param add_year convert t into year. Either "t30" or the name of a mapping
#' in \code{time_mappings}.
#' @param year_mapping a mapping table to translate t into year. Reserved for
#' future use; currently ignored, as \code{add_year} selects the mapping.
#' @param ... additional parameters to send to batch_extract
#'
#' @returns A \code{data.table} with the index columns of \code{item} and a
#' \code{value} column, restricted to \code{filter}. A \code{scenario} column is
#' added when \code{scenarios} is not \code{NULL}, and a \code{year} column when
#' \code{add_year} is not \code{NULL} and the item has a \code{t} index. The
#' \code{gdx} and \code{t} columns are dropped unless \code{keep_gdx},
#' respectively \code{keep_t}, is TRUE.
#'
#' @export
witch_query <- function(item,
                        resgdx,
                        filter = list(),
                        scenarios = guess_scenario(resgdx),
                        add_year = "t30",
                        keep_gdx = FALSE,
                        keep_t = FALSE,
                        year_mapping = witch_period_year,
                        valigdx = NULL,
                        histgdx = NULL,
                        ...) {

  # Load item from resgdx
  .tab <- gdxtools::batch_extract(item[1], resgdx, ...)[[1]]
  data.table::setDT(.tab)

  all_ids <- names(.tab)[names(.tab) != "value"]

  # Filter

  ## Split idx with ","
  sidx <- lapply(filter, function(x, pattern)
    stringr::str_split(x, pattern = pattern)[[1]], ",")

  ## Keep n index separated
  nsidx <- sidx[['n']]

  # Check if there is aggregated regions
  agg_world <- "world" %in% nsidx
  n_filter <- nsidx[!nsidx %in% c("world")]

  .restab <- NULL

  # Aggregate World
  if (agg_world) {
    sidx[['n']] <- NULL

    # Filter according to selection
    if (length(sidx) > 0) {
      .tabx <- .tab[do.call(pmin, Map(`%in%`, .tab[, names(sidx), with = FALSE],
                                     sidx)) == 1L]
    } else {
      .tabx <- .tab
    }

    all_ids_wo_n <- all_ids[all_ids != "n"]
    .tabx <- .tabx[, lapply(.SD, sum), by = all_ids_wo_n, .SDcols = "value"]
    .tabx[, n := "world"]

    .restab <- c(.restab, list(.tabx))

  }

  # Filter n
  if (is.null(nsidx) | length(n_filter) > 0) {

    sidx[['n']] <- n_filter

    # Filter according to selection
    if (length(sidx) > 0) {
      .tabx <- .tab[do.call(pmin, Map(`%in%`, .tab[, names(sidx), with = FALSE],
                                      sidx)) == 1L]
      .restab <- c(.restab, list(.tabx))
    } else {
      .restab <- c(.restab, list(.tab))
    }

  }

  # Collect regional aggregation and filter
  .tab <- data.table::rbindlist(.restab, use.names = TRUE)

  # Add year
  if (!is.null(add_year) & "t" %in% names(.tab)) {
    if (add_year == "t30") {
      .tab[, year := as.numeric(t) * 5 + 2000]
    } else {
      tm <- time_mappings[[add_year]]
      if (!is.null(tm)) {
        .tab <- merge(.tab, tm[, .(t, year = as.numeric(refyear))], by = "t")
      }
    }
  }

  # Associate scenarios
  if (!is.null(scenarios)) {
    s_map <- data.table::data.table(gdx = resgdx, scenario = scenarios)
    .tab <- merge(.tab, s_map, by = "gdx")
  }

  # Clean gdx
  if (!keep_gdx) {
    .tab[, gdx := NULL]
  }

  # Clean t
  if (!keep_t & "t" %in% names(.tab)) {
    .tab[, t := NULL]
  }

  # Return final table
  return(.tab)

}
