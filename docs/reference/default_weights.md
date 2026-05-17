# Default weights.

A collection of country-level Statistics to be used as weights for
regional disaggregation/aggregation. Values are given for 250 ISO3.

## Usage

``` r
default_weights
```

## Format

A list of data.table:

- pop:

  Population in 2005

- gdp:

  GDP in 2005

- co2:

  CO2 FFI in 2005

- extr_coal_2000:

  Coal extraction in 2000

- extr_gas_2000:

  Gas extraction in 2000

- extr_oil_2000:

  Oil extraction in 2000

- tpes_2005:

  TPES in 2005

- prodelec_2005:

  Electricity production in 2005

- prodelec_hydro_2005:

  Electricity production from hydroplants in 2005

- extr_oil_gas_2000:

  Extraction of oil and gas in 2000

- forest:

  Forest cover

- land:

  Forest cover

- cst:

  constant

## Source

<https://tntcat.iiasa.ac.at/SspDb>,
<https://www.pik-potsdam.de/paris-reality-check/primap-hist/>,
<http://cait.wri.org/>,
<https://www.iea.org/reports/world-energy-outlook-2018>,
<https://databank.worldbank.org/source/world-development-indicators>
