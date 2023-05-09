#' WITCH palettes for WITCH results
#'
#' Creates thematic palettes for WITCH results
#'
#' @family WITCH helper functions
#' @seealso \code{\link{witch_query}}, \code{\link{witch_scen}}.
#'
#' @param components  .
#' @param theme A theme palette name.
#' @param restrict only return the values with name in this vector.
#' @export
#'
witch_pal <- function(theme = NULL, restrict = NULL) {

  # Set to default theme palette
  if (is.null(theme)) {
    warning('theme is NULL')
    return(NULL)
  }

  if (theme == "fuel") {
    default_cols <- c(coal = "#3e3e3e",
                      ngas  = "#659AC5",
                      nuclear = "#8E61E8",
                      oil = "#663E28",
                      solar = "#FFE205",
                      wind = "#252C8F")
    if (is.null(include_names)) {
      return(default_cols)
    }

    # select names from include_names
    cols <- default_cols[[include_names]]

    # Check similar names
    if (c("gas","natural_gas") %in% include_names) {
      #cols = c(cols, )
    }

    return(cols)
  }

}
