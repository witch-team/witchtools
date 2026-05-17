# List of WITCH results files

Returns a vector of WITCH results files. Inspired by witch-plot source
code.

## Usage

``` r
witch_results_files(
  search_path,
  restrict = "^results_",
  normalize = FALSE,
  recursive = FALSE
)
```

## Arguments

- search_path:

  Path where to start the search of the results files
  (default="^results\_").

- restrict:

  vector of filtering patterns on the file names.

- normalize:

  if TRUE, return canonical form of the file names.

- recursive:

  if TRUE, search also in the sub directories.

## See also

[witch_query](http://witchtools.witchmodel.org/reference/witch_query.md).

Other WITCH helper functions:
[`witch_query()`](http://witchtools.witchmodel.org/reference/witch_query.md),
[`witch_scen_name()`](http://witchtools.witchmodel.org/reference/witch_scen_name.md)
