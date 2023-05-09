#' Fast query of WITCH results files
#'
#' Returns a data.table containing the result of the query with additional
#' columns on scenario
#'
#' @family WITCH helper functions
#' @seealso \code{\link{witch_scen}}, \code{\link{witch_pal}}.
#'
#' @param item parameter or variable name
#' @param resgdx list of results gdx from WITCH
#' @param filter named list of queries
#' @param add_scen convert gdx into scenario name
#' @param scen_table a conversion function or a mapping table to translate gdx into scenario name
#' @param add_year convert t into year
#' @param year_mapping a mapping table to translate t into year
#'
#' @export
witch_query <- function(item = NULL,
                        resgdx = NULL,
                        filter = list(),
                        add_scenario = TRUE,
                        scenario_mapping = witch_scen_name,
                        add_year = TRUE,
                        year_mapping = witch_period_year,
                        valigdx = NULL,
                        histgdx = NULL,
                        agg_n = NULL,
                        clean_columns = TRUE,
                        ...) {

  if (is.null(item)) {
    warning('needs item!')
    return(NULL)
  }

  if (is.null(resgdx)) {
    warning('needs resgdx!')
    return(NULL)
  }

  # Load results gdx
  .tab <- gdxtools::batch_extract(item, resgdx, ...)[[1]]
  data.table::setDT(.tab)

  # all ids to filter, including those for aggregation
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
