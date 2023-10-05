#' Fast query of WITCH results files
#'
#' Returns a formatted data.table from a list of results files.
#' It adds a scenario column
#'
#' @family WITCH helper functions
#'
#' @param item parameter or variable name
#' @param resgdx list of WITCH results gdx
#' @param filter named list of filter (eg. list(e="CO2",n="brazil"))
#' @param scenarios vector of scenario names in same order than resgdx
#' @param keep_gdx
#' @param add_scen convert gdx into scenario name
#' @param scen_table a conversion function or a mapping table to translate gdx into scenario name
#' @param add_year convert t into year
#' @param year_mapping a mapping table to translate t into year
#' @param ... additional parameters sent to batch_extract
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
                        agg_n = NULL,
                        add_scenario = TRUE,
                        ...) {

  # Load item from resgdx
  .tab <- gdxtools::batch_extract(item[1], resgdx, ...)[[1]]
  data.table::setDT(.tab)

  all_ids <- names(.tab)[names(.tab) != "value"]

  # Filter

  ## Split idx with ","
  sidx <- lapply(filter, function(x, pattern)
    stringr::str_split(x, pattern = pattern)[[1]], ",")

  # Filter according to selection
  if (length(sidx) > 0) {
    .tab <- .tab[do.call(pmin, Map(`%in%`, .tab[, names(sidx), with = FALSE],
                                   sidx)) == 1L]
  }

  # Add year
  if (!is.null(add_year) & "t" %in% names(.tab)) {
    if (add_year == "t30") {
      .tab[, year := as.numeric(t) * 5 + 2000]
    } else {
      tm <- time_mappings[[add_year]]
      if (!is.null) {
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

  if (F) {

    # all ids to filter
    idx <- filter
    sidx <- idx

    # Split idx with ","
    sidx <- lapply(sidx, function(x, pattern) stringr::str_split(x, pattern = pattern)[[1]], ",")

    # Region specific queries
    if( "n" %in% names(idx)){
      ## If query is "n = world", select all n
      if (idx[['n']] == "world") {
        sidx[['n']] <- NULL
      }
    }

    # Filter according to selection
    if (length(sidx) > 0) {
      .tab <- .tab[do.call(pmin, Map(`%in%`, .tab[, names(sidx), with = FALSE], sidx)) == 1L]
    }

    # Aggregate

    ## World region
    if( "n" %in% names(idx)){
      if (idx[['n']] == "world") {
        bycol <- names(.tab)[!names(.tab) %in% c("value","n")]
        .tab <- .tab[, .(n = "world", value = sum(value)), by = bycol]
      }
    }

    # Time
    if (add_year) {
      if(is.function(year_mapping)) {
        .tab[, year := year_mapping(gdx)]
      } else if(is.data.frame(year_mapping)) {
        stopifnot("year_mapping should have columns named year and t" = all(c("t","year") %in% names(year_mapping)))
        .tab = merge(.tab, year_mapping, by = "t", all.x = TRUE)
      } else {
        warning("year_mapping should be a function or a data.frame")
      }
    }

    # Scenario
    if (add_scenario) {
      if(is.function(scenario_mapping)) {
        .tab[, scenario := scenario_mapping(gdx)]
      } else if(is.data.frame(scenario_mapping)) {
        stopifnot("scenario_mapping should have column named gdx and scenarios" = all(c("gdx","scenario") %in% names(scenario_mapping)))
        .tab = merge(.tab, scenario_mapping, by = "gdx", all.x = TRUE)
      } else {
        warning("scenario_mapping should be a function or a data.frame")
      }
    }

    # Cleaning
    if (clean_columns) {
      .tab[, c("t","gdx") := NULL]
    }

  }

}
