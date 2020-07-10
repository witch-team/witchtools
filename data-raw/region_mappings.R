# default region mappings

library(data.table)


load_region_mapping <- function(f) {
  region_inc_file <- readLines(f)
  region_inc_file <- region_inc_file[region_inc_file != ""] # Remove empty lines
  region_inc_file <- region_inc_file[!stringr::str_detect(region_inc_file, "^\\*")] # Remove * comments
  region_inc_file <- stringr::str_trim(stringr::str_split_fixed(region_inc_file, "#", 2)[, 1]) # Remove # comments

  if (length(region_inc_file[stringr::str_detect(region_inc_file, "set map_*")]) == 0) {
    return(NULL)
  }

  set.begin <- grep("set map_*", tolower(region_inc_file))[1]
  set.end <- set.begin + grep(";", region_inc_file[set.begin:length(region_inc_file)])[1]
  region_inc_map <- region_inc_file[(set.begin + 1):(set.end - 2)]
  region_inc_map <- stringr::str_split(region_inc_map, "\\.")
  region_inc_map <- data.table::data.table(matrix(unlist(region_inc_map), ncol = 2, byrow = TRUE))
  region_inc_map[, V1 := tolower(V1)]
  region_inc_map[, V2 := toupper(V2)]
  data.table::setnames(region_inc_map, c(stringr::str_sub(basename(f), 1, -5), "iso3"))

  return(region_inc_map)
}

# Region mappings
region_mapping_files <- Sys.glob(file.path("data-raw", "regions", "*.inc"))
region_mappings <- lapply(region_mapping_files, load_region_mapping)
names(region_mappings) <- stringr::str_sub(basename(region_mapping_files), 1, -5)

# Check that iso3 are consistent across all region mappings
stopifnot(length(unique(unlist(lapply(region_mappings, function(x) x$iso3)))) ==
  unique(unlist(lapply(region_mappings, nrow))))

usethis::use_data(region_mappings, compress = "xz", overwrite = T)
