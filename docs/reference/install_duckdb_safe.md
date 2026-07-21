# Install duckdb package with proper handling

Install duckdb package with proper handling

## Usage

``` r
install_duckdb_safe()
```

## Value

Invisibly `NULL`. Called for its side effect of installing the `duckdb`
package from r-universe and CRAN; throws an error if the package is
still unavailable afterwards.
