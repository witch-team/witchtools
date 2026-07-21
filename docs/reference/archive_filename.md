# Return a name for a archive name, based on time-region mappings and git commit information.

Return a name for a archive name, based on time-region mappings and git
commit information.

## Usage

``` r
archive_filename(reg_id, time_id, ext = ".zip")
```

## Arguments

- reg_id:

  a character for the region id.

- time_id:

  a character for the time id.

- ext:

  the extension of the archive file (default:".zip").

## Value

A character string with the archive file name, of the form
`data_<reg_id>_<time_id>_<commit_id>_<commit_date><ext>`.
