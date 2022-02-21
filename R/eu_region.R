#' EU regions for a given region mapping.
#'
#' \code{eu_region} returns a vector of region representing Europe. The
#' selection is based on the GDP of countries. If the total GDP of EU
#' countries from the region is greater than 50% of total GDP of the region,
#' the region is considered as belonging of Europe.
#'
#' @family misc functions
#'
#' @param region_mapping a data.table of regional mapping.
#' @return a vector of region name.
#'
#' @export
#' @examples
#' eu_regions(region_mappings[["witch17"]])
eu_regions <- function(region_mapping) {
  reg_id <- region_id(region_mapping)
  eu_iso3 <- c(

    "ALA",
    "ALB",
    "AND",
    "AUT",
    "BEL",
    "BIH",
    "BGR",
    "DEU",
    "CZE",
    "CYP",
    "DNK",
    "ESP",
    "EST",
    "FIN",
    "FRA",
    "FRO",
    "GBR",
    "GRC",
    "GRL",
    "HRV",
    "HUN",
    "IRL",
    "ISL",
    "KSV",
    "LUX",
    "LVA",
    "LTU",
    "MKD",
    "MLT",
    "MNE",
    "NLD",
    "NOR",
    "POL",
    "PRT",
    "ROU",
    "SRB",
    "SJM",
    "SVK",
    "SVN",
    "SWE"
  )
  tab <- merge(
    region_mapping,
    data.table::data.table(iso3 = eu_iso3, eu = 1),
    by = "iso3",
    all.x = TRUE
  )
  tab[is.na(eu), eu := 0]
  tab <- merge(tab, witchtools::default_weights[["gdp"]], by = "iso3")
  tab <- tab[, list(is_eu = sum(weight * eu) / sum(weight)), by = reg_id]
  threshold <- 0.5
  return(tab[is_eu > threshold, get(reg_id)])
}
