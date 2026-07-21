# Install a package with proper handling of non-interactive sessions

Install a package with proper handling of non-interactive sessions

## Usage

``` r
install_package_safe(pkg)
```

## Arguments

- pkg:

  package name as a character string

## Value

Invisibly `NULL`. Called for its side effect of installing `pkg` from
CRAN; throws an error if the package is still unavailable afterwards.
