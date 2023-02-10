#' Scenario name from the GDX filename
#'
#' Returns a scenario name
#'
#' @family WITCH helper functions
#' @seealso \code{\link{witch_query}}, \code{\link{witch_pal}}.
#'
#' @export
witch_scen_name <- function(gdxfilename) {

  scen <- basename(gdxfilename)
  scen <- stringr::str_replace(scen,"results_ssp\\d_", "")
  scen <- stringr::str_replace(scen,".gdx", "")

  return(scen)

}

