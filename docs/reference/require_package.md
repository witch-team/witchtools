# Install, if necessary, and load R libraries

Install, if necessary, and load R libraries

## Usage

``` r
require_package(pkgs, loading = TRUE)
```

## Arguments

- pkgs:

  package names as a character vector

- loading:

  if TRUE, package is loaded

## Examples

``` r
if (FALSE) { # \dontrun{
require_package(c("data.table","gdxtools"), loading = FALSE)
} # }
```
