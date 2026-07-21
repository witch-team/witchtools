# Convert a data.table from a regional mapping to another.

`convert_region` returns a list containing a data.table where values are
converted from one regional mapping to another. The conversion is done
by downscaling values at country-level (if necessary) and updscaling
them into the final regional mapping. The list also contain an optional
data.table about the value coperture for the operator `sumby`. The
required format of the input data is described in Details.

## Usage

``` r
convert_region(
  .x,
  from_reg = NULL,
  to_reg,
  agg_operator = "sum",
  agg_weight = witchtools::default_weights[["gdp"]],
  agg_missing = "NA",
  regions = witchtools::region_mappings,
  info = FALSE
)
```

## Arguments

- .x:

  a well-formatted data.table.

- from_reg:

  initial regional mapping name or a data.table with the mapping.

- to_reg:

  final regional mapping name or a data.table with the mapping.

- agg_operator:

  aggregation operator (See Details for the list of possible values)

- agg_weight:

  aggregation weight data.table (See Details for the list of possible
  values)

- agg_missing:

  tells how to deal with missing values ("NA" or "zero")

- regions:

  optional list of region mappings (see Details for format)

- info:

  logical indicating whether to include information, only required for
  the agg_operator "sumby".

## Value

a list containing a converted data.table and information about the
coperture if available.

## Details

The input data.table should contain a column "value" and a column named
by the initial regional mapping `from_reg`, while the other columns are
are considered as id columns. The resulting data.table have a column
named by the final regional mapping `to_reg`.

The regional mapping for `from_reg` and `to_reg` can be provided in a
named list through the parameter `regions`. Regional mappings are
2-columns data.table with a column named 'iso3' (for country ISO3) and
another one named as the regional mapping (for region name). The name in
the list should also be the regional mapping name.

## See also

[`convert_table`](http://witchtools.witchmodel.org/reference/convert_table.md),
[`convert_gdx`](http://witchtools.witchmodel.org/reference/convert_gdx.md).

Other conversion functions:
[`convert_duckdb()`](http://witchtools.witchmodel.org/reference/convert_duckdb.md),
[`convert_gdx()`](http://witchtools.witchmodel.org/reference/convert_gdx.md),
[`convert_sqlite()`](http://witchtools.witchmodel.org/reference/convert_sqlite.md),
[`convert_table()`](http://witchtools.witchmodel.org/reference/convert_table.md),
[`convert_time_period()`](http://witchtools.witchmodel.org/reference/convert_time_period.md)

## Examples

``` r
# Aggregate country-level GDP into the 17 WITCH regions
gdp_iso3 <- data.table::copy(default_weights[["gdp"]])
data.table::setnames(gdp_iso3, "weight", "value")

convert_region(gdp_iso3, to_reg = "witch17")
#>         witch17      value
#>          <char>      <num>
#>  1:        laca  3811.0076
#>  2:       sasia  2616.5509
#>  3:         ssa  3281.0841
#>  4:      europe 22137.4325
#>  5:        mena  7253.0736
#>  6:          te  8156.9139
#>  7:     oceania  1501.8456
#>  8:      brazil  2969.7366
#>  9:      seasia  4873.5174
#> 10:      canada  1746.1213
#> 11:       china 22961.7494
#> 12:   indonesia  3118.3992
#> 13:       india  8377.0372
#> 14:      jpnkor  7243.6577
#> 15:      mexico  2300.2179
#> 16:         usa 19910.1964
#> 17: southafrica   749.2962
```
