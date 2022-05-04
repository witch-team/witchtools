#' Fast query of WITCH results files
#'
#' Returns a data.table containing the result of the query with additional
#' columns on scenario
#' @param item parameter or variable name
#' @param idx named list of queries
#'
#' @export
witch_query <- function(item, idx, resgdx,
                        scen = TRUE, scen_fun = witch_scen_name,
                        valigdx = NULL, histgdx = NULL,
                        clean = TRUE) {

  # Load results gdx
  .tab <- gdxtools::batch_extract(item, resgdx)[[1]]
  data.table::setDT(.tab)

  # Update idx
  sidx <- idx

  # Split idx with ","
  sidx <- lapply(sidx, function(x, pattern) stringr::str_split(x, pattern = pattern)[[1]], ",")

  ## If query is "n = world", select all n
  if (idx[['n']] == "world") {
    sidx[['n']] <- NULL
  }

  # Filter according to selection
  .tab <- .tab[do.call(pmin, Map(`%in%`, .tab[, names(sidx), with = FALSE], sidx)) == 1L]

  # Aggregate

  ## World region
  if (idx[['n']] == "world") {
    bycol <- names(.tab)[!names(.tab) %in% c("value","n")]
    .tab <- .tab[, .(n = "world", value = sum(value)), by = bycol]
  }

  # Convert
  .tab[, year := as.numeric(t) * 5 + 2000]

  # Scenario
  if (scen) {
    .tab[, scen := scen_fun(gdx)]
  }

  # Clean some columns
  if (clean) {
    .tab[, c("t","gdx") := NULL]
  }


}
