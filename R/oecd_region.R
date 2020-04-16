#' OECD regions for a given region mapping.
#'
#' \code{oecd_region} returns a vector of region representing OECD. The
#' selection is based on the GDP of countries. If the total GDP of OECD
#' countries from the region is greater than 50% of total GDP of the region,
#' the region is considered as belonging of the OECD.
#'
#' @family misc functions
#'
#' @param region_mapping a data.table of regional mapping.
#' @return a vector of region name.
#'
#' @export
#' @examples
#' oecd_regions(region_mappings[['witch17']])
#'

oecd_regions <- function(region_mapping) {
  reg_id <- region_id(region_mapping)
  oecd_iso3 <- c(
    "AUS",
    "AUT",
    "BEL",
    "CAN",
    "CHL",
    "CZE",
    "DNK",
    "EST",
    "FIN",
    "FRA",
    "DEU",
    "GRC",
    "HUN",
    "ISL",
    "IRL",
    "ISR",
    "ITA",
    "JPN",
    "KOR",
    "LUX",
    "MEX",
    "NLD",
    "NZL",
    "NOR",
    "POL",
    "PRT",
    "SVK",
    "SVN",
    "ESP",
    "SWE",
    "CHE",
    "TUR",
    "GBR",
    "USA"
  )
  tab <- merge(
    region_mapping,
    data.table::data.table(iso3 = oecd_iso3, oecd = 1),
    by = "iso3",
    all.x = TRUE
  )
  tab[is.na(oecd), oecd := 0]
  tab <- merge(tab, witchtools::default_weights[['gdp']], by = "iso3")
  tab <- tab[, list(is_oecd = sum(weight * oecd) / sum(weight)), by = reg_id]
  threshold <- 0.5
  return(tab[is_oecd > threshold, get(reg_id)])
}
