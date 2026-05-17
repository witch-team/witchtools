# Write the IAMC regional mapping to common regions for WITCH yml file as in https://github.com/IAMconsortium/common-definitions

`write_region_mappings_yml` write a yaml file with the region mappings.

## Usage

``` r
write_region_mappings_yml(
  filename = NULL,
  model = "WITCH 5.0",
  n = "witch17",
  comm_regs = c("world", "r5", "r9", "r10")
)
```

## Arguments

- filename:

  yaml filename to write

- model:

  model version

- n:

  Witch region definition. THE REGION MUST BE DEFINED IN WTCHTOOLS.

- comm_regs:

  Additional mappings, which region aggregate, to add.

## Author

Lara Aleluia Reis
