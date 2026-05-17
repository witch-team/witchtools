# Convert a data.table from year to time period.

`convert_time_period` returns a formatted data.table aggregating values
from years into time periods. The required format of the input data is
described in Details.

## Usage

``` r
convert_time_period(
  .x,
  time_mapping,
  do_interp = FALSE,
  do_extrap = FALSE,
  do_past_extrap = FALSE,
  year_name = "year",
  value_name = "value",
  time_aggregate = "mean",
  na.rm = TRUE,
  verbose = FALSE
)
```

## Arguments

- .x:

  a well-formatted data.table.

- time_mapping:

  a time mapping data.table.

- do_interp:

  logical indicating whether linear interpolation should be done.

- do_extrap:

  logical indicating whether constant extrapolation should be done.

- do_past_extrap:

  logical indicating whether constant extrapolation should be done for
  past value.

- year_name:

  string column name of year.

- value_name:

  string column name of value.

- time_aggregate:

  function to aggregate yearly values ("mean","sum","max").

- na.rm:

  logical indicating whether missing values should be removed.

- verbose:

  logical indicating whether running in verbose mode.

## Value

a converted data.table

## Details

The input data.table should contain the columns "year" and "value",
while the other columns are are considered as id columns. The resulting
data.table will have a column "t" instead of "year. Note that the
period, year and value column names can be specified.

The time mapping between year and time period should be provided as a
data.table with columns "year" and "t", and refyear for interpolation
and extrapolation. In case of missing periods, these can be linearly
interpolated or constantly interpolated. If several years are mapped
into the same period, values are averaged or the function
`fun.aggregate` is used.

## See also

[`convert_table`](http://witchtools.witchmodel.org/reference/convert_table.md),
[`convert_gdx`](http://witchtools.witchmodel.org/reference/convert_gdx.md).

Other conversion functions:
[`convert_duckdb()`](http://witchtools.witchmodel.org/reference/convert_duckdb.md),
[`convert_gdx()`](http://witchtools.witchmodel.org/reference/convert_gdx.md),
[`convert_region()`](http://witchtools.witchmodel.org/reference/convert_region.md),
[`convert_sqlite()`](http://witchtools.witchmodel.org/reference/convert_sqlite.md),
[`convert_table()`](http://witchtools.witchmodel.org/reference/convert_table.md)

## Examples

``` r
library(data.table)
#> 
#> Attaching package: ‘data.table’
#> The following object is masked from ‘package:base’:
#> 
#>     %notin%

# original data.table
dd <- data.table(year = 2005:2050, value = 1:46)

# Convert yearly time-serie into time period
convert_time_period(dd, "t30")
#>          t value
#>     <char> <num>
#>  1:      1     2
#>  2:      2     6
#>  3:      3    11
#>  4:      4    16
#>  5:      5    21
#>  6:      6    26
#>  7:      7    31
#>  8:      8    36
#>  9:      9    41
#> 10:     10    45
```
