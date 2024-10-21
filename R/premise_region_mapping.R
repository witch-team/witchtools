#' PREMISE region mapping.
#'
#' \code{premise_region_mapping} returns a json containing a regional
#' mapping to be used be PREMISE. If the filename is not provided, it
#' will return the json as text.
#'
#' @param n regional mapping ID
#' @param filename the name of the file to save the json
#'
#' @export
#' @examples
#' \dontrun{
#' premise_region_mapping(filename = "witch-topology.json")
#' }
#'
premise_region_mapping <- function(n = 'witch17',
                                   filename = NULL){

  # Get the region mapping
  region_list <- witchtools::region_mappings[[n]]

  # Get iso2 from iso3 and fix Kosovo code
  region_list$iso3 <- countrycode::countrycode(
    as.vector(region_list$iso3),
    origin = 'iso3c',
    destination = 'iso2c',
    custom_match = c("KSV" = "XK")
  )

  # Group by region
  json_data <- split(region_list$iso3, region_list$witch17)

  # Add world region
  if (is.null(json_data$world)) {
    json_data$world <- c("GLO", "RoW")
  }

  # Convert to json
  json_data <- jsonlite::toJSON(json_data, pretty = TRUE)

  # Write to file or print
  if (is.null(filename)) {
    cat(json_data)
  } else {
    write(json_data, file = filename)
  }

  invisible(NULL)

}
