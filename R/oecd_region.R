# oecd_region

oecd_regions = function(reg_id,
                        region_mappings,
                        weights,
                        threshold = 0.5,
                        w = "gdp") {
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
  tab = merge(
    region_mappings[[reg_id]],
    data.table(iso3 = oecd_iso3, oecd = 1),
    by = "iso3",
    all.x = T
  )
  tab[is.na(oecd), oecd := 0]
  tab = merge(tab, weights[[w]], by = "iso3")
  tab = tab[, .(is_oecd = sum(weight * oecd) / sum(weight)), by = reg_id]
  return(tab[is_oecd > threshold, get(reg_id)])
}
