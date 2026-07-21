# Batch convert tables from a SQLite database

`convert_sqlite` writes a converted SQLite database in the
`output_directory`. All tables from the input `sqlitedb` are converted
using the `convert_table` function. Specific conversion options are read
in the parameter `meta_param` also stored in the `sqlitedb`.

## Usage

``` r
convert_sqlite(
  sqlitedb,
  reg_id,
  time_id,
  output_directory,
  region_mappings = witchtools::region_mappings,
  time_mappings = witchtools::time_mappings,
  weights = witchtools::default_weights,
  guess_input_t = "t30",
  region_name = NULL,
  guess_region = "witch17",
  default_agg_missing = "NA",
  default_meta_param = NULL
)
```

## Arguments

- sqlitedb:

  SQLITE file.

- reg_id:

  final regional aggregation.

- time_id:

  final time_period aggregation.

- output_directory:

  directory where to write the converted SQLITE DB

- region_mappings:

  a named list of region mapping data.table.

- time_mappings:

  a named list of time mapping data.table.

- weights:

  a named list of weights used by `convert_region`

- guess_input_t:

  input time mapping if not explicitely defined

- region_name:

  column name of the region, reg_id if null

- guess_region:

  input regional mapping if not explicitely defined

- default_agg_missing:

  default parameter to handle missing values in `convert_region`

- default_meta_param:

  default meta_param

## Value

Called for its side effect of writing the converted SQLite database in
`output_directory`, under the same base name as `sqlitedb`. The return
value is the (invisible) status of the closing `RSQLite::dbDisconnect`
call and should not be relied upon.

## See also

[`convert_table`](http://witchtools.witchmodel.org/reference/convert_table.md),
[`convert_gdx`](http://witchtools.witchmodel.org/reference/convert_gdx.md).

Other conversion functions:
[`convert_duckdb()`](http://witchtools.witchmodel.org/reference/convert_duckdb.md),
[`convert_gdx()`](http://witchtools.witchmodel.org/reference/convert_gdx.md),
[`convert_region()`](http://witchtools.witchmodel.org/reference/convert_region.md),
[`convert_table()`](http://witchtools.witchmodel.org/reference/convert_table.md),
[`convert_time_period()`](http://witchtools.witchmodel.org/reference/convert_time_period.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Not run: needs an existing SQLite database file.
convert_sqlite("input/build/data_climate.sqlite", "witch17", "t30", "data_witch17")
} # }
```
