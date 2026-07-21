# Install, if necessary, and load the gdxtools library

Install, if necessary, and load the gdxtools library

## Usage

``` r
require_gdxtools(loading = TRUE)
```

## Arguments

- loading:

  if TRUE, package is loaded

## Value

Invisibly `NULL`. Called for its side effects of installing `gdxtools`
(\>= 1.0.0) from GitHub if needed and, when `loading` is TRUE, attaching
it to the search path.

## Examples

``` r
if (FALSE) { # \dontrun{
# Not run: installs gdxtools from GitHub as a side effect.
require_gdxtools()
} # }
```
