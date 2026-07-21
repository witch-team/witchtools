# Read time mapping from a csv file.

`witch_time_mapping` reads a time csv file and returns the time mapping
as defined in the file. The function scans the csv file.

## Usage

``` r
witch_time_mapping(f)
```

## Arguments

- f:

  time mapping csv file

## Value

A `data.table` with one row per (t, year) pair and the columns `t`,
`year` (numeric), `refyear`, `pred`, `tperiod`, `begyear` and `endyear`.

## Examples

``` r
# Read the t30 time mapping shipped with the package
witch_time_mapping(
  system.file("extdata", "t30.csv", package = "witchtools")
)
#>           t  year refyear   pred tperiod begyear endyear
#>      <char> <num>  <char> <char>  <char>  <char>  <char>
#>   1:      1  2003    2005              1    2003    2007
#>   2:      1  2004    2005              1    2003    2007
#>   3:      1  2005    2005              1    2003    2007
#>   4:      1  2006    2005              1    2003    2007
#>   5:      1  2007    2005              1    2003    2007
#>  ---                                                    
#> 146:     30  2148    2150     29      30    2148    2152
#> 147:     30  2149    2150     29      30    2148    2152
#> 148:     30  2150    2150     29      30    2148    2152
#> 149:     30  2151    2150     29      30    2148    2152
#> 150:     30  2152    2150     29      30    2148    2152
```
