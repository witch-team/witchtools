# Write the IAMC native regions in yaml as defined in https://github.com/IAMconsortium/common-definitions

`iamc_native_regions_yml` write a yaml file with the native regions.

## Usage

``` r
iamc_native_regions_yml(filename = NULL, model = "WITCH 5.0", n = "witch17")
```

## Arguments

- filename:

  yaml filename to write

- model:

  model version

- n:

  Witch region definition, as defined in witchtools.

## Value

Invisibly `NULL`. Called for its side effect of writing the native
regions yaml file to `filename`, or to `native_regions_<model>.yml` in
the working directory when `filename` is `NULL`.

## Author

Lara Aleluia Reis
