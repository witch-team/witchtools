# Default timescale mapping

A collection of timescale mappings.

## Usage

``` r
time_mappings
```

## Format

A named list of data.tables with 7 columns

- t:

  time period as a ordered numeric ID

- year:

  time year

- refyear:

  reference year for the time period

- pred:

  predecessor ID (t) of the time period

- tperiod:

  time step of the time period, used for stochastic branch, equal to t
  when deterministic

- begyear:

  first year of the time period

- endyear:

  last year of the time period
