#' Scenario name from the GDX filename
#'
#' Returns a scenario name
#'
#' @family WITCH helper functions
#'
#' @param gdxfilename path to a results gdx file.
#'
#' @returns A character string with the scenario name.
#'
#' @export
witch_scen_name <- function(gdxfilename) {

  scen <- basename(gdxfilename)
  scen <- stringr::str_replace(scen,"results_ssp\\d_", "")
  scen <- stringr::str_replace(scen, "\\.gdx$", "")

  return(scen)

}

