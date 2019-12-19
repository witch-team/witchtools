#' Load weights using witch-data folder
#'
#' @param idir input data directory, where data are downloaded or already present
#' @param region_mappings named list of region mappings
#'
#' @export
#' @import data.table
#' @examples
#' \dontrun{
#' region_mapping_files = Sys.glob(file.path(system.file("regions", package = "witchtools"),"*.inc"))
#' region_mappings <- lapply(region_mapping_files, load_region_mapping)
#' names(region_mappings) <- stringr::str_sub(basename(region_mapping_files), 1, -5)
#' load_weights('../witch-data',load_region_mapping(region_mappings))
#' }
#'
load_weights <- function(idir,region_mappings){

  # All these ISO3 should be informed
  iso3_list = unique(unlist(lapply(region_mappings, function(x)x$iso3)))

  w <- list()

  # load from gdx
  if (requireNamespace('gdxtools', quietly = TRUE)) {

    # population_ssp2_2005
    piggyback::pb_download(repo = 'witch-team/witch-data',tag = 'v0.0.1',file = 'ssp-ssp_gdp_pop.gdx', dest = idir)
    mygdx <- gdxtools::gdx(file.path(idir, "ssp-ssp_gdp_pop.gdx"))
    pop <- data.table(mygdx["pop_base_oecd"])
    setnames(pop, 1:4, c("ssp", "iso3", "year", "value"))
    w = c(w, list(population_ssp2_2005 = pop[year == 2005 &
                                               ssp == "SSP2", .(iso3, weight = value)]))

    # gdp_ssp2_2005
    piggyback::pb_download(repo = 'witch-team/witch-data',tag = 'v0.0.1',file = 'ssp-ssp_gdp_pop.gdx', dest = idir)
    mygdx <- gdxtools::gdx(file.path(idir, "ssp-ssp_gdp_pop.gdx"))
    gdp <- data.table(mygdx["gdp_base_oecd"])
    setnames(gdp, 1:4, c("ssp", "iso3", "year", "value"))
    w = c(w, list(gdp_ssp2_2005 = gdp[year == 2005 &
                                        ssp == "SSP2", .(iso3, weight = value)]))

    w = c(w, list(pop = w[["population_ssp2_2005"]]))
    w = c(w, list(gdp = w[["gdp_ssp2_2005"]]))

  }

  # co2ffi_emissions_2005
  piggyback::pb_download(repo = 'witch-team/witch-data',tag = 'v0.0.1',file = 'primap-primap-hist.sqlite', dest = idir)
  sqldb <-
    RSQLite::dbConnect(RSQLite::SQLite(), dbname = file.path(idir, 'primap-primap-hist.sqlite'))
  hemi = as.data.table(RSQLite::dbGetQuery(sqldb, 'select * from primap'))
  RSQLite::dbDisconnect(sqldb)
  emi_gwp_ch4 = 25
  emi_gwp_n2o = 298
  co2_ffi <-
    hemi[entity == "CO2" & category %in% c("CAT1", "CAT2") &
           year == 2005,
         .(e = "co2ffi", value = sum(value, na.rm = TRUE) * 12 / 44 *
             1e-6), by = c("year", "iso3")]
  w = c(w, list(co2ffi_emissions_2005 = co2_ffi[, .(iso3, weight = value)]))
  ch4_lu <-
    hemi[entity == "CH4" &
           category %in% c("CAT4", "CAT5")  & year == 2005,
         .(e = "ch4lu",
           value = sum(value, na.rm = TRUE) * emi_gwp_ch4 * 12 / 44 * 1e-6), by = c("year", "iso3")]
  w = c(w, list(ch4lu_emissions_2005 = ch4_lu[, .(iso3, weight = value)]))
  n2o_lu <-
    hemi[entity == "N2O" & category %in% c("CAT4", "CAT5") &
           year == 2005,
         .(e = "n2olu",
           value = sum(value, na.rm = TRUE) * emi_gwp_n2o * 12 / 44 * 1e-6), by = c("year", "iso3")]
  w = c(w, list(n2olu_emissions_2005 = n2o_lu[, .(iso3, weight = value)]))

  # wbio_2005
  piggyback::pb_download(repo = 'witch-team/witch-data',tag = 'v0.0.1',file = 'weo-weo2018_energy_balances.csv', dest = idir)
  weo = fread(file.path(idir, 'weo-weo2018_energy_balances.csv'))
  w = c(w, list(wbio_2010 = weo[var == "Q_PES_WBIO" &
                                  time == 2010, .(iso3, weight = value)]))
  w = c(w, list(extr_coal_2000 = weo[var == "Q_OUT_COAL" &
                                       time == 2000, .(iso3, weight = value)]))
  w = c(w, list(extr_gas_2000 = weo[var == "Q_OUT_GAS" &
                                      time == 2000, .(iso3, weight = value)]))
  w = c(w, list(extr_oil_2000 = weo[var == "Q_OUT_OIL" &
                                      time == 2000, .(iso3, weight = value)]))
  w = c(w, list(tpes_2005 = weo[var == "tpes" &
                                  time == 2005, .(iso3, weight = value)]))
  w = c(w, list(prodelec_2005 = weo[var == "Q_EN_EL" &
                                      time == 2005, .(iso3, weight = value)]))
  w = c(w, list(prodelec_hydro_2005 = weo[var == "Q_EN_ELHYDRO" &
                                            time == 2005, .(iso3, weight = value)]))

  oil_gas_out = merge(weo[var == "Q_OUT_GAS" &
                            time == 2000, .(iso3, weight = value)], weo[var == "Q_OUT_OIL" &
                                                                          time == 2000, .(iso3, weight = value)], by = 'iso3', all = TRUE)
  oil_gas_out[, weight := weight.x + weight.y]
  oil_gas_out[, c('weight.x', 'weight.y') := NULL]
  w = c(w, list(extr_oil_gas_2000 = oil_gas_out))

  #add weights from CAIT
  piggyback::pb_download(repo = 'witch-team/witch-data',tag = 'v0.0.1',file = 'wri-world_resources_institute_cait.csv', dest = idir)
  ghg.cait = fread(file.path(idir, 'wri-world_resources_institute_cait.csv'),
                   header = TRUE)
  ghg.cait = ghg.cait[!is.na(iso3) &
                        !is.na(GHG), .(iso3, weight = GHG)]
  w = c(w, list(ghg_cait = ghg.cait))

  #add weights from WDI
  piggyback::pb_download(repo = 'witch-team/witch-data',tag = 'v0.0.1',file = 'wdi-wdi_variables.csv', dest = idir)
  wdi = fread(file.path(idir, 'wdi-wdi_variables.csv'))
  w = c(w, split(wdi[year == 2005, .(iso3, weight = value)], wdi[year ==
                                                                   2005]$variable))

  #add weights from WEO
  piggyback::pb_download(repo = 'witch-team/witch-data',tag = 'v0.0.1',file = 'imf-weo_variables.csv', dest = idir)
  weo = fread(file.path(idir, 'imf-weo_variables.csv'))
  weo = split(weo[year == 2005, .(iso3, weight = value)], weo[year == 2005]$variable)
  names(weo)  = stringr::str_c(names(weo), "_2005_weo")
  w = c(w, weo)

  # Shortcuts
  #now all iso3 regions for full sums
  w = c(w, list(cst = data.table(iso3 = iso3_list, weight = 1)))
  w = c(w, list(co2 = w[["co2ffi_emissions_2005"]]))

  # Make weights consistent
  tidy_weights <- function(dd) {
    dd <- rbind(dd, data.table(iso3 = iso3_list[!iso3_list %in% dd$iso3]), fill = TRUE)
    dd <- dd[iso3 %in% iso3_list]
    dd[is.na(weight), weight := 1e-10]
    dd[weight == 0, weight := 1e-10]
  }
  w = lapply(w, tidy_weights)

  return(w)

}
