# Handler for witch-data files in WITCH

Returns the location of a file from the witch-data repository for
processing, method (`local`) or after checking/downloading it from the
DVC remote drive (method `dvc`).

## Usage

``` r
witch_data(
  file,
  version = NULL,
  idir = getOption("witchtools.idir"),
  method = getOption("witchtools.method"),
  noCheck = getOption("witchtools.noCheck", TRUE),
  repo = getOption("witchtools.witch_data_repo"),
  remote = getOption("witchtools.witch_data_remote")
)
```

## Arguments

- file:

  Name of the file in the with-data repository

- version:

  File version, only required for method piggyback (deprecated)

- idir:

  directory to read/download files

- method:

  'dvc' or 'local'.

- noCheck:

  don't check and download the file.

- repo:

  github repository name

- remote:

  dvc remote storage

## Examples

``` r
if (FALSE) { # \dontrun{
witch_data("ssp/ssp_population.csv", version = "v0.0.1")
witch_data("ssp/ssp_population.csv", method = "witch-data")
witch_data("ssp/ssp_population.csv", noCheck = TRUE)
} # }
```
