# Read region mapping from a regional WITCH GAMS file.

`witch_region_mapping` reads a regional WITCH GAMS file and returns the
regional mapping as defined in the file. The function scans the GAMS
file with the assumptions that each iso3 is on one line and in a set
named map\_\*.

## Usage

``` r
witch_region_mapping(f)
```

## Arguments

- f:

  Regional WITCH GAMS file

## Value

A `data.table` with two columns: the region name, named after the base
name of `f` without its extension, and `iso3`. Returns `NULL` if no
`map_*` set is found in the file.

## Examples

``` r
# Read the WITCH 17-region mapping shipped with the package
witch_region_mapping(
  system.file("extdata", "witch17.inc", package = "witchtools")
)
#>      witch17   iso3
#>       <char> <char>
#>   1:  canada    CAN
#>   2:  canada    SPM
#>   3:  jpnkor    JPN
#>   4:  jpnkor    KOR
#>   5: oceania    NZL
#>  ---               
#> 246:      te    TUR
#> 247:      te    TKM
#> 248:      te    UKR
#> 249:      te    UZB
#> 250:     usa    USA
```
