# witchtools 0.4.0
- Remove DVC support as default for `witch_data`. Now, just return the filename.

# witchtools 0.3.8
- Add renewables capacities and generation weights

# witchtools 0.3.7

- Add region aggregations (gcam32, witch18rus)
- Fix issue with NA and mean aggregation

# witchtools 0.3.6

- Add new weights for landuse

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
