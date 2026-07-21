# Upload a file as an asset in a release of a github repository (deprecated).

Upload a file as an asset in a release of a github repository
(deprecated).

## Usage

``` r
witch_data_upload(file, version = NULL, method = NULL, repo = NULL)
```

## Arguments

- file:

  Name of the file in the with-data repository

- version:

  NULL

- method:

  NULL.

- repo:

  NULL

## Value

Invisibly `NULL`. The function is deprecated and only emits a
deprecation warning; it performs no upload.

## Examples

``` r
if (FALSE) { # \dontrun{
# Not run: uploads to the witch-data repository (deprecated).
witch_data_upload("ssp/ssp_population.csv", version = "v0.0.1")
} # }
```
