# Convert time periods and regions in a table.

`convert_table` returns a list containing a data.table where values are
converted from years into time periods and from one regional mapping to
another. The function is calling `convert_time_period` and
`convert_region`. More details about the parameters in these functions.
The input table might be a data.table or a data.frame and should contain
a column "value". All others columns are considered as indices.

## Usage

``` r
convert_table(
  .x,
  ...,
  options = list(),
  time_mapping = NULL,
  from_reg = NULL,
  to_reg = NULL,
  agg_weight = NULL,
  regions = NULL,
  do_time_period = TRUE,
  do_region = TRUE,
  info = FALSE
)
```

## Arguments

- .x:

  a well-formatted data.table.

- ...:

  parameters to send to convert_region or convert_time_period.

- options:

  a list of parameters to send to convert_region or convert_time_period.

- time_mapping:

  a time mapping data.table.

- from_reg:

  initial regional mapping name or a data.table with the mapping.

- to_reg:

  final regional mapping name or a data.table with the mapping.

- agg_weight:

  aggregation weight data.table

- regions:

  optional list of region mappings (see Details for format)

- do_time_period:

  logical indicating whether years should be converted.

- do_region:

  logical indicating whether region should be converted.

- info:

  logical indicating whether to include information, only necessary when
  the agg_operator is "sumby".

## Value

a data.table or list containing a converted data.table and information
about the coperture if available.

## See also

[`convert_gdx`](http://witchtools.witchmodel.org/reference/convert_gdx.md)
for WITCH gdx files.

Other conversion functions:
[`convert_duckdb()`](http://witchtools.witchmodel.org/reference/convert_duckdb.md),
[`convert_gdx()`](http://witchtools.witchmodel.org/reference/convert_gdx.md),
[`convert_region()`](http://witchtools.witchmodel.org/reference/convert_region.md),
[`convert_sqlite()`](http://witchtools.witchmodel.org/reference/convert_sqlite.md),
[`convert_time_period()`](http://witchtools.witchmodel.org/reference/convert_time_period.md)

## Examples

``` r
if (FALSE) { # \dontrun{

convert_table(gdp_iso3, to_reg = "witch17", time_mapping = "t30")
} # }
```
