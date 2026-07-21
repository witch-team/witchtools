# PREMISE region mapping.

`premise_region_mapping` builds a json containing a regional mapping to
be used by PREMISE. It is written to `filename`, or printed to the
console if no filename is provided.

## Usage

``` r
premise_region_mapping(n = "witch17", filename = NULL)
```

## Arguments

- n:

  regional mapping ID

- filename:

  the name of the file to save the json

## Value

Invisibly `NULL`. Called for its side effect of writing the json mapping
to `filename`, or of printing it to the console when `filename` is
`NULL`.

## Examples

``` r
# Write the PREMISE topology for the default region mapping
premise_region_mapping(filename = tempfile(fileext = ".json"))
```
