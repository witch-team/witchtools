#' Region ID of a regional mapping.
#'
#' \code{region_id} returns the region id of a region mapping.
#'
#' @family misc functions
#'
#' @param region_mapping a data.table of regional mapping.
#' @return a character for the regional mapping ID.
#'
#' @export
#' @examples
#' region_id(region_mappings[["witch17"]])
region_id <- function(region_mapping) {
  return(names(region_mapping)[names(region_mapping) != "iso3"][1])
}
