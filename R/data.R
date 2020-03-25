NULL

#' Default weights.
#'
#' A collection of country-level Statistics to be used as weights
#' for regional disaggregation/aggregation. Values are given for 250 ISO3.
#'
#' @format A list of data.table:
#' \describe{
#'   \item{pop}{Population in 2005}
#'   \item{gdp}{GDP in 2005}
#'   \item{co2}{CO2 FFI in 2005}
#'   \item{extr_coal_2000}{Coal extraction in 2000}
#'   \item{extr_gas_2000}{Gas extraction in 2000}
#'   \item{extr_oil_2000}{Oil extraction in 2000}
#'   \item{tpes_2005}{TPES in 2005}
#'   \item{prodelec_2005}{Electricity production in 2005}
#'   \item{prodelec_hydro_2005}{Electricity production from hydroelectric plants in 2005}
#'   \item{extr_oil_gas_2000}{Extraction of oil and gas in 2000}
#'   \item{forest}{Forest cover}
#'   \item{land}{Forest cover}
#'   \item{cst}{constant}
#'
#' }
#' @source \url{https://tntcat.iiasa.ac.at/SspDb},
#' \url{https://www.pik-potsdam.de/paris-reality-check/primap-hist/},
#' \url{http://cait.wri.org/},
#' \url{https://www.iea.org/reports/world-energy-outlook-2018},
#' \url{https://databank.worldbank.org/source/world-development-indicators}
"default_weights"

#' Default regional mapping
#'
#' A collection of regional mappings for 250 ISO3.
#'
#' @format A named list of data.tables with 2 columns
#' \describe{
#'   \item{mapping_name}{Mapping name (example, witch17),
#'   contains the region names in lower case}
#'   \item{iso3}{ISO3 code in upper case}
#' }
"region_mappings"

#' Default timescale mapping
#'
#' A collection of timescale mappings.
#'
#' @format A named list of data.tables with 7 columns
#' \describe{
#'   \item{t}{time period as a ordered numeric ID}
#'   \item{year}{time year}
#'   \item{refyear}{reference year for the time period}
#'   \item{pred}{predecessor ID (t) of the time period}
#'   \item{tperiod}{time step of the time period, used for stochastic branch,
#'   equal to t when deterministic}
#'   \item{begyear}{first year of the time period}
#'   \item{endyear}{last year of the time period}
#' }
"time_mappings"
