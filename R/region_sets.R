#' Brazil regions for a given region mapping.
#'
#' \code{brazil_regions} returns a vector of region representing Brazil.
#'
#' @family misc functions
#'
#' @param region_mapping a data.table of regional mapping.
#' @return a vector of region name.
#'
#' @export
#' @examples
#' brazil_regions(region_mappings[["witch17"]])
brazil_regions <- function(region_mapping) {
  usa_iso3 <- c("BRA")
  return(any_regions(usa_iso3,region_mapping))
}

#' USA regions for a given region mapping.
#'
#' \code{usa_regions} returns a vector of region representing USA.
#'
#' @family misc functions
#'
#' @param region_mapping a data.table of regional mapping.
#' @return a vector of region name.
#'
#' @export
#' @examples
#' usa_regions(region_mappings[["witch17"]])
usa_regions <- function(region_mapping) {
  usa_iso3 <- c("USA")
  return(any_regions(usa_iso3,region_mapping))
}

#' China regions for a given region mapping.
#'
#' \code{china_regions} returns a vector of region representing China,
#' including Honk Kong and Macao.
#'
#' @family misc functions
#'
#' @param region_mapping a data.table of regional mapping.
#' @return a vector of region name.
#'
#' @export
#' @examples
#' china_regions(region_mappings[["witch17"]])
china_regions <- function(region_mapping) {
  china_iso3 <- c("CHN","HKG","MAC")
  return(any_regions(china_iso3,region_mapping))
}

#' India regions for a given region mapping.
#'
#' \code{india_regions} returns a vector of region representing China.
#'
#' @family misc functions
#'
#' @param region_mapping a data.table of regional mapping.
#' @return a vector of region name.
#'
#' @export
#' @examples
#' india_regions(region_mappings[["witch17"]])
india_regions <- function(region_mapping) {
  india_iso3 <- c("IND")
  return(any_regions(india_iso3,region_mapping))
}

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
    "ITA",
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
  return(any_regions(eu_iso3,region_mapping))
}

#' EU27 regions for a given region mapping.
#'
#' \code{eu27_region} returns a vector of region representing EU27 member states. The
#' selection is based on the GDP of countries. If the total GDP of EU27
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
#' eu27_regions(region_mappings[["witch17"]])
eu27_regions <- function(region_mapping) {
  eu27_iso3 <- c('AUT', 'BEL', 'BGR', 'HRV', 'CYP', 'CZE', 'DNK', 'EST',
                 'FIN', 'FRA', 'DEU', 'GRC', 'HUN', 'IRL', 'ITA', 'LVA',
                 'LTU', 'LUX', 'MLT', 'NLD', 'POL', 'PRT', 'ROU', 'SVK',
                 'SVN', 'ESP', 'SWE')
  return(any_regions(eu27_iso3,region_mapping))
}

#' EU28 regions for a given region mapping.
#'
#' \code{eu28_region} returns a vector of region representing the former EU28 member states. The
#' selection is based on the GDP of countries. If the total GDP of EU28
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
#' eu28_regions(region_mappings[["witch17"]])
eu28_regions <- function(region_mapping) {
  eu28_iso3 <- c('AUT', 'BEL', 'BGR', 'HRV', 'CYP', 'CZE', 'DNK', 'EST',
                 'FIN', 'FRA', 'DEU', 'GRC', 'HUN', 'IRL', 'ITA', 'LVA',
                 'LTU', 'LUX', 'MLT', 'NLD', 'POL', 'PRT', 'ROU', 'SVK',
                 'SVN', 'ESP', 'SWE', 'GBR')
  return(any_regions(eu28_iso3,region_mapping))
}

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
#' oecd_regions(region_mappings[["witch17"]])
oecd_regions <- function(region_mapping) {
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
  return(any_regions(oecd_iso3,region_mapping))
}


any_regions <- function(iso3_set, region_mapping) {
  reg_id <- region_id(region_mapping)
  tab <- merge(
    region_mapping,
    data.table::data.table(iso3 = iso3_set, sel = 1),
    by = "iso3",
    all.x = TRUE
  )
  tab[is.na(sel), sel := 0]
  tab <- merge(tab, witchtools::default_weights[["gdp"]], by = "iso3")
  tab <- tab[, list(is_sel = sum(weight * sel) / sum(weight)), by = reg_id]
  threshold <- 0.5
  return(sort(tab[is_sel > threshold, get(reg_id)]))
}
