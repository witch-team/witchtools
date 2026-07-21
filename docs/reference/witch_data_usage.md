# Wrapper for the usage of a make_data file in WITCH for future use of witch_data

Returns the list containing the options as returned by docopt, the
method name `method` and the location of the input data directory. It
also updates the witchtools options, according to the call parameters.

## Usage

``` r
witch_data_usage(file)
```

## Arguments

- file:

  Name of the make data file

## Value

A named list of command-line options as returned by
[`docopt`](https://rdrr.io/pkg/docopt/man/docopt.html), augmented with
the elements `witch_dir`, `idir` and `method`. As a side effect, the
options `witchtools.idir`, `witchtools.method` and `witchtools.noCheck`
are set.

## Examples

``` r
if (FALSE) { # \dontrun{
# Not run: needs a make_data script.
opts <- witch_data_usage("make_data_validation.R")
} # }
```
