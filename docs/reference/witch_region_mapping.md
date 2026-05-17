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

## Examples

``` r
if (FALSE) { # \dontrun{
witch_region_mapping("input/regions/witch17.inc")
} # }
```
