#' PREMISE region mapping.
#'
#' \code{premise_region_mapping} builds a json containing a regional
#' mapping to be used by PREMISE. It is written to \code{filename}, or
#' printed to the console if no filename is provided.
#'
#' @param n regional mapping ID
#' @param filename the name of the file to save the json
#'
#' @returns Invisibly \code{NULL}. Called for its side effect of writing the
#' json mapping to \code{filename}, or of printing it to the console when
#' \code{filename} is \code{NULL}.
#'
#' @export
#' @examples
#' # Write the PREMISE topology for the default region mapping
#' premise_region_mapping(filename = tempfile(fileext = ".json"))
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
  json_data <- split(region_list$iso3, region_list[[n]])

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
