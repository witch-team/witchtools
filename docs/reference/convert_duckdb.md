# Batch convert tables from a DuckDB database

`convert_duckdb` writes a converted DuckDB database in the
`output_directory`. All tables from the input `duckdb` are converted
using the `convert_table` function. Specific conversion options are read
in the parameter `meta_param` also stored in the `duckdb`.

## Usage

``` r
convert_duckdb(
  duckdb,
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

- duckdb:

  DuckDB file.

- reg_id:

  final regional aggregation.

- time_id:

  final time_period aggregation.

- output_directory:

  directory where to write the converted DuckDB DB

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

## See also

[`convert_table`](http://witchtools.witchmodel.org/reference/convert_table.md),
[`convert_gdx`](http://witchtools.witchmodel.org/reference/convert_gdx.md),
[`convert_sqlite`](http://witchtools.witchmodel.org/reference/convert_sqlite.md).

Other conversion functions:
[`convert_gdx()`](http://witchtools.witchmodel.org/reference/convert_gdx.md),
[`convert_region()`](http://witchtools.witchmodel.org/reference/convert_region.md),
[`convert_sqlite()`](http://witchtools.witchmodel.org/reference/convert_sqlite.md),
[`convert_table()`](http://witchtools.witchmodel.org/reference/convert_table.md),
[`convert_time_period()`](http://witchtools.witchmodel.org/reference/convert_time_period.md)

## Examples

``` r
if (FALSE) { # \dontrun{
convert_duckdb("input/build/data_climate.duckdb", "witch17", "t30", "data_witch17")
} # }
```
