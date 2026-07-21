# Translate WITCH data (run make_data files and convert data).

`witch_translate_data` generates the input data for the WITCH model.
First, it will run the R `make_data_files` and the gams
`make_data_files` usually located in the input folder of WITCH. These
make_data_files produce gdx, sqlite, and duckdb files in the `build`
folder. Time-series should be yearly. Spatial data should be preferably
described at ISO3 level, but the script will handle any regional-mapping
contained in `regions`.

## Usage

``` r
witch_translate_data(
  witch_dir = ".",
  region,
  timescale,
  idir = NULL,
  output_dir = NULL,
  regions = region_mappings,
  times = time_mappings,
  force = FALSE
)
```

## Arguments

- witch_dir:

  WITCH main directory

- region:

  final regional aggregation

- timescale:

  final timescale aggregation

- idir:

  input data folder (for weights and to be pass to make_data files)

- output_dir:

  output folder (to overidde default WITCH data folder name)

- regions:

  optional list of regional mappings (see Details for format)

- times:

  optional list of timescale mappings (see Details for format)

- force:

  logical indicating whether all make files should be processed

## Value

Invisibly `NULL`. Called for its side effects of running the `make_data`
files, writing the converted gdx, SQLite and DuckDB files in the output
data directory, and generating the associated GAMS files via
`witch_write_gams`.

## Details

The regional mappings should be provided in a named list through the
parameter `regions`. Regional mappings are 2-columns data.table with a
column named 'iso3' (for country ISO3) and another one named as the
regional mapping (for region name). The name in the list should also be
the regional mapping name.

`times` is a list of time mapping between year and time period which
should be provided as a data.table with columns "year" and "t", and
refyear for interpolation and extrapolation. `times` should contain
`timescale`

## Examples

``` r
if (FALSE) { # \dontrun{
# Not run: needs a WITCH model directory.
witch_translate_data(region = "r5", timescale = "t30")
} # }
```
