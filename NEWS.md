# witchtools (development version)

# witchtools 0.5.1

## Bug fixes

- `convert_gdx()` could not be called without explicitly passing
  `region_mappings` and `time_mappings`. Both defaults referred to the
  parameters themselves rather than to the package data, so any other call
  failed with "promise already under evaluation". The documented example
  could not run.
- `witch_query()` failed for any `add_year` other than `"t30"`: a malformed
  `if (!is.null)` test (missing its argument) raised "invalid argument type".
- `witch_translate_data()` could not generate its GAMS include files.
  `witch_write_gams()` ignored the time mapping it was given and read an
  undefined `time_id` instead, erroring after writing part of the output.
- `premise_region_mapping()` ignored its `n` argument and always grouped by
  the `witch17` column, so any other mapping failed with "group length is 0".
- `guess_scenario()` returned the whole path as the scenario name when given
  Windows-style paths on Linux or macOS, where `\` is not a path separator.
- `witch_scen_name()` and `guess_scenario()` treated `".gdx"` as a regular
  expression, stripping the first four-character run ending in `gdx` anywhere
  in the name instead of the extension.
- `witch_results_files(normalize = TRUE)` resolved file names against the
  working directory rather than `search_path`, so it warned and returned
  un-normalized names whenever the two differed.
- `archive_store()` could not overwrite an existing archive on macOS or
  Windows: the pre-copy delete targeted the wrong path, so `fs::file_copy()`
  failed with `EEXIST`.

## Other changes

- Loading the package no longer overrides `witchtools.*` options that the
  user has already set.
- `remotes` moved from `Imports` to `Suggests`; it is only needed to install
  `gdxtools` from GitHub and was already installed on demand.
- Removed the unused internal `paleiee()` palette helper, which referenced a
  `witch_regions` dataset that does not exist and could not run.

# witchtools 0.5.0
- Require `gdxtools (>= 1.0.0)` — the gamstransfer-backed release.
  `require_package("gdxtools")` now re-installs from GitHub when the
  local copy is older than 1.0.0.

# witchtools 0.4.13
- Add `convert_duckdb` function for batch conversion of DuckDB databases
- Add DBI and duckdb as optional dependencies (Suggests)
- Update `witch_translate_data` to automatically process DuckDB files (`data_*.duckdb`)
- Improve `require_package` to automatically install missing packages in non-interactive sessions
- Add helpful error messages when no writable R library directory is available

# witchtools 0.4.12
- Use rlang::check_installed to check for and to install required packages

# witchtools 0.4.11
- Add weights for Air Pollution

# witchtools 0.4.10
- Add function to write regions mapping for IAMC submission

# witchtools 0.4.9
- Better definition for eu set
- Create `is_europe` for European Countries
- Update ed58 mapping from RICE50x

# witchtools 0.4.8
- In GAMS, add 2 coalition variables to deal with a high number of coalitions.
- Add witch20, ed57 aggregation

# witchtools 0.4.7
- Add 'tagg' in metaparam to control the function to aggregate over time.

# witchtools 0.4.6
- Speed up conversion
- Functions to archive the data directory

# witchtools 0.4.5 
- Add witch_query function with basic query capabilities
- Add guess_scenario function to guess the scenario name from a list of gdx
- Add some regional sets (brazil, china...)

# witchtools 0.4.4
- Write yaml for native regions and mappings for the IIASA database
- Add r10 region aggregation, based on limits10
- Add r9 region aggregation

# witchtools 0.4.3
- fix bug in pop and gdp being equal in default_weights

# witchtools 0.4.2
- Add weights for mod_forestry (`gsv` and `wood_harvest`)
- Add region 'witchglobiom15'
- Add weight 'gdpcap'

# witchtools 0.4.1
- Fix a bug on loading region mapping files with non-empty lines.
- Fix `witch_data_usage` default method, set as local.
- Default option `witchtools.noCheck` to `TRUE`.

# witchtools 0.4.0
- Remove DVC support as default for `witch_data`. Now, just return the filename.

# witchtools 0.3.8
- Add renewables capacities and generation weights

# witchtools 0.3.7

- Add region aggregations (gcam32, witch18rus)
- Fix issue with NA and mean aggregation

# witchtools 0.3.6

- Add new weights for land use

# witchtools 0.3.5

- Add RICEx50 time and region aggregation
- Add regions set `eu`, `eu27`, `eu28`, `iso3eur`
- Update `convert_region` to allow downscaling with `sumby`. It was not possible before.
- Add iso3eur regional mapping.

# witchtools 0.3.4

- Add `witch_data_usage`

# witchtools 0.3.3

- Add `witch34` region mapping

# witchtools 0.3.2

- Add `witch_time_mapping` function

# witchtools 0.3.1

- Fix warning and missing quote when a space is in the filename

# witchtools 0.3.0

- Implements DVC by default for witch-data

# witchtools 0.2.3

-   Add IEA region mapping
-   Add some witch default parameters

# witchtools 0.2.2

-   Add witch_read_mapping to read WITCH regional mapping file

# witchtools 0.2.1

-   add ghg_cait weight

# witchtools 0.2.0

-   Refactor the main functions for general use
-   Add documentation
-   Add a website

# witchtools 0.1.2

-   Fix use of multi subdirectories in witch-data

# witchtools 0.1.1

-   Add piggyback support

# witchtools 0.1.0

-   First release version based on the WITCH source code
-   Add witch18eu region aggregation
-   convert globiom `ch4lu` to `ch4_agr`
