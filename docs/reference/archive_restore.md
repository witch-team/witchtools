# Try to save the archive in a list of directories.

Try to save the archive in a list of directories.

## Usage

``` r
archive_restore(filename, dir_list, extract_dir = ".")
```

## Arguments

- filename:

  the name of the archive to save.

- dir_list:

  a list of potential directories where to save the archive.

- extract_dir:

  the directory where to unzip the archive

## Value

A character string with the path of the first archive found, which has
been unzipped into `extract_dir`; invisibly `NULL` if the archive was
not found in any of the directories of `dir_list`.
