# Try to save the archive in a list of directories.

Try to save the archive in a list of directories.

## Usage

``` r
archive_store(filename, dir_list)
```

## Arguments

- filename:

  the name of the archive to save.

- dir_list:

  a list of potential directories where to save the archive.

## Value

Invisibly `NULL`. Called for its side effect of copying `filename` into
each existing directory of `dir_list`.
