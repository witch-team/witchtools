# default_weights

library(gdxtools)
library(data.table)
library(witchtools)

# All these ISO3 should be informed
iso3_list <- unique(unlist(lapply(region_mappings, function(x) x$iso3)))

w <- list()

## pop
# Definition: 2005 population [millions]
# Source: SSP database v1
mygdx <- gdx("data-raw/ssp-ssp_gdp_pop.gdx")
pop <- setDT(mygdx["pop_base_oecd"])
setnames(pop, 1:4, c("ssp", "iso3", "year", "value"))
w <- c(w, list(pop = pop[year == 2005 & ssp == "SSP2", .(iso3, weight = value)]))

## gdp
# Definition: 2005 GDP [T USD2005]
# Source: SSP database v1
gdp <- setDT(mygdx["gdp_base_oecd"])
setnames(gdp, 1:4, c("ssp", "iso3", "year", "value"))
w <- c(w, list(gdp = gdp[year == 2005 & ssp == "SSP2", .(iso3, weight = value)]))

sqldb <-
  RSQLite::dbConnect(RSQLite::SQLite(),
    dbname = "data-raw/primap-primap-hist.sqlite"
  )
hemi <- setDT(RSQLite::dbGetQuery(sqldb, "select * from primap"))
RSQLite::dbDisconnect(sqldb)

emi_gwp_ch4 <- 25
emi_gwp_n2o <- 298

## co2
co2_ffi <-
  hemi[entity == "CO2" & category %in% c("CAT1", "CAT2") &
    year == 2005,
  .(e = "co2ffi", value = sum(value, na.rm = TRUE) * 12 / 44 *
    1e-6),
  by = c("year", "iso3")
  ]
w <- c(w, list(co2 = co2_ffi[, .(iso3, weight = value)]))

#
ch4_lu <-
  hemi[entity == "CH4" &
    category %in% c("CAT4", "CAT5") & year == 2005,
  .(
    e = "ch4lu",
    value = sum(value, na.rm = TRUE) * emi_gwp_ch4 * 12 / 44 * 1e-6
  ),
  by = c("year", "iso3")
  ]
w <- c(w, list(ch4lu_emissions_2005 = ch4_lu[, .(iso3, weight = value)]))

n2o_lu <-
  hemi[entity == "N2O" & category %in% c("CAT4", "CAT5") &
    year == 2005,
  .(
    e = "n2olu",
    value = sum(value, na.rm = TRUE) * emi_gwp_n2o * 12 / 44 * 1e-6
  ),
  by = c("year", "iso3")
  ]
w <- c(w, list(n2olu_emissions_2005 = n2o_lu[, .(iso3, weight = value)]))

# wbio_2005
weo <- fread("data-raw/weo-weo2018_energy_balances.csv")
w <- c(w, list(wbio_2010 = weo[var == "Q_PES_WBIO" &
  time == 2010, .(iso3, weight = value)]))
w <- c(w, list(extr_coal_2000 = weo[var == "Q_OUT_COAL" &
  time == 2000, .(iso3, weight = value)]))
w <- c(w, list(extr_gas_2000 = weo[var == "Q_OUT_GAS" &
  time == 2000, .(iso3, weight = value)]))
w <- c(w, list(extr_oil_2000 = weo[var == "Q_OUT_OIL" &
  time == 2000, .(iso3, weight = value)]))
w <- c(w, list(tpes_2005 = weo[var == "tpes" &
  time == 2005, .(iso3, weight = value)]))
w <- c(w, list(prodelec_2005 = weo[var == "Q_EN_EL" &
  time == 2005, .(iso3, weight = value)]))
w <- c(w, list(prodelec_hydro_2005 = weo[
  var == "Q_EN_ELHYDRO" &
    time == 2005,
  .(iso3, weight = value)
]))

oil_gas_out <- merge(weo[var == "Q_OUT_GAS" &
  time == 2000, .(iso3, weight = value)],
weo[var == "Q_OUT_OIL" &
  time == 2000, .(iso3, weight = value)],
by = "iso3", all = TRUE
)
oil_gas_out[, weight := weight.x + weight.y]
oil_gas_out[, c("weight.x", "weight.y") := NULL]
w <- c(w, list(extr_oil_gas_2000 = oil_gas_out))

# add weights from CAIT
f <- "data-raw/world_resources_institute_cait.csv"
ghg.cait <- fread(f, header = TRUE)
ghg.cait <- ghg.cait[!is.na(iso3) &
  !is.na(GHG), .(iso3, weight = GHG)]
w <- c(w, list(ghg_cait = ghg.cait))

# add weights from WDI
wdi <- fread("data-raw/wdi-wdi_variables.csv")
w <- c(w, split(
  wdi[year == 2005, .(iso3, weight = value)],
  wdi[year == 2005]$variable
))

# add weights from WEO
weo <- data.table::fread("data-raw/imf-weo_variables.csv")
weo <- split(
  weo[year == 2005, .(iso3, weight = value)],
  weo[year == 2005]$variable
)
names(weo) <- stringr::str_c(names(weo), "_2005_weo")
w <- c(w, weo)

# Shortcuts
# now all iso3 regions for full sums
w <- c(w, list(cst = data.table(iso3 = iso3_list, weight = 1)))

# Add land-use (km2)
f <- "data-raw/hildaplus_landuse_cover_2022.csv"
hp <- fread(f)

cforest <- hp[variable == "cover_forest" & year == 2015, .(iso3, weight = value)]
ccrop <- hp[variable == "cover_crop" & year == 2015, .(iso3, weight = value)]
cpasture <- hp[variable == "cover_pasture" & year == 2015, .(iso3, weight = value)]
curban <- hp[variable == "cover_urban" & year == 2015, .(iso3, weight = value)]
totarea <- hp[variable == "cover_total" & year == 2015, .(iso3, weight = value)]

# Add LULUC CO2 emissions (GtC)
f <- "data-raw/eluc_oscar_2022.csv"
co2lu <- fread(f)
xco2lu <- co2lu[year %in% c(2013:2017), .(weight = mean(value)), by = "iso3"]

w <- c(w, list(hildap_cover_forest = cforest,
               hildap_cover_cropland = ccrop,
               hildap_cover_pasture = cpasture,
               hildap_cover_urban = curban,
               hildap_total_area = totarea,
               oscar_co2lu = xco2lu))

# Make weights consistent
tidy_weights <- function(dd) {
  dd <- rbind(dd, data.table(iso3 = iso3_list[!iso3_list %in% dd$iso3]),
    fill = TRUE
  )
  dd <- dd[iso3 %in% iso3_list]
  dd[is.na(weight), weight := 1e-10]
  dd[weight == 0, weight := 1e-10]
}
w <- lapply(w, tidy_weights)


# check used weights in WITCH code
# build_dir <- "/home/lolow/Seafile/WITCH/witch/input/build/"
# f <- Sys.glob(file.path(build_dir,'*.gdx'))
# metap <- setDT(batch_extract('meta_param',f)[[1]])
# witch_weights <- unique(metap[V2 == "nweight",V3])

witch_weights <- c(
  "cst", "gdp", "co2", "forest", "land", "pop", "extr_oil_gas_2000",
  "extr_coal_2000", "prodelec_hydro_2005", "extr_oil_2000",
  "extr_gas_2000", "prodelec_2005", "tpes_2005",
  "wbio_2010", "ch4lu_emissions_2005", "n2olu_emissions_2005",
  "agland", "ghg_cait",
  "hildap_cover_forest",
  "hildap_cover_cropland",
  "hildap_cover_pasture",
  "hildap_cover_urban",
  "hildap_total_area",
  "oscar_co2lu"
)

default_weights <- w[names(w) %in% witch_weights]

usethis::use_data(default_weights, compress = "xz", overwrite = T)
