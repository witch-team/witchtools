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

  convert t into year. Either "t30" or the name of a mapping in
  `time_mappings`.

- keep_gdx:

  keep gdx file name in the result

- keep_t:

  keep t in the result

- year_mapping:

  a mapping table to translate t into year. Reserved for future use;
  currently ignored, as `add_year` selects the mapping.

- valigdx:

  optional gdx file with validation data. Reserved for future use;
  currently ignored.

- histgdx:

  optional gdx file with historical data. Reserved for future use;
  currently ignored.

- ...:

  additional parameters to send to batch_extract

## Value

A `data.table` with the index columns of `item` and a `value` column,
restricted to `filter`. A `scenario` column is added when `scenarios` is
not `NULL`, and a `year` column when `add_year` is not `NULL` and the
item has a `t` index. The `gdx` and `t` columns are dropped unless
`keep_gdx`, respectively `keep_t`, is TRUE.

## See also

Other WITCH helper functions:
[`witch_results_files()`](http://witchtools.witchmodel.org/reference/witch_results_files.md),
[`witch_scen_name()`](http://witchtools.witchmodel.org/reference/witch_scen_name.md)
