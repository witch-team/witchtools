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

## Value

Invisibly `NULL`. Called for its side effects of installing the missing
packages of `pkgs` and, when `loading` is TRUE, attaching them to the
search path.

## Examples

``` r
if (FALSE) { # \dontrun{
# Not run: installs and attaches packages as a side effect.
require_package(c("data.table","gdxtools"), loading = FALSE)
} # }
```
