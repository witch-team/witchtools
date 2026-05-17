# PREMISE region mapping.

`premise_region_mapping` returns a json containing a regional mapping to
be used be PREMISE. If the filename is not provided, it will return the
json as text.

## Usage

``` r
premise_region_mapping(n = "witch17", filename = NULL)
```

## Arguments

- n:

  regional mapping ID

- filename:

  the name of the file to save the json

## Examples

``` r
if (FALSE) { # \dontrun{
premise_region_mapping(filename = "witch-topology.json")
} # }
```
