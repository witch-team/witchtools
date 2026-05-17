# Fast query of WITCH results files

Returns a formatted data.table from a list of results files. It adds a
scenario column

## Usage

``` r
witch_query(
  item,
  resgdx,
  filter = list(),
  scenarios = guess_scenario(resgdx),
  add_year = "t30",
  keep_gdx = FALSE,
  keep_t = FALSE,
  year_mapping = witch_period_year,
  valigdx = NULL,
  histgdx = NULL,
  ...
)
```

## Arguments

- item:

  parameter or variable name

- resgdx:

  list of WITCH results gdx

- filter:

  named list of filter (eg. list(e="CO2",n="brazil,usa")). if n contains
  "world", then the sum of n is computed.

- scenarios:

  vector of scenario names in same order than resgdx

- add_year:

  convert t into year

- keep_gdx:

  keep gdx file name in the result

- keep_t:

  keep t in the result

- year_mapping:

  a mapping table to translate t into year

- ...:

  additional parameters to send to batch_extract

- scen_table:

  a conversion function or a mapping table to translate gdx into
  scenario name

## See also

Other WITCH helper functions:
[`witch_results_files()`](http://witchtools.witchmodel.org/reference/witch_results_files.md),
[`witch_scen_name()`](http://witchtools.witchmodel.org/reference/witch_scen_name.md)
