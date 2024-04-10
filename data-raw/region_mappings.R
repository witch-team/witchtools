# default region mappings

library(data.table)

print("Extract mappings")

# Region mappings
region_mapping_files <- Sys.glob(file.path("data-raw", "regions", "*.inc"))
region_mappings <- lapply(region_mapping_files, witchtools::witch_region_mapping)
names(region_mappings) <- stringr::str_sub(basename(region_mapping_files), 1, -5)

# Check that iso3 are consistent across all region mappings
stopifnot(length(unique(unlist(lapply(region_mappings, function(x) x$iso3)))) ==
  unique(unlist(lapply(region_mappings, nrow))))

usethis::use_data(region_mappings, compress = "xz", overwrite = T)

print("Extract descriptions")

load_region_description <- function(f) {
  region_inc_file <- readLines(f)
  region_inc_file <- region_inc_file[region_inc_file != ""] # Remove empty lines
  region_inc_file <- region_inc_file[!stringr::str_detect(region_inc_file, "^\\*")] # Remove * comments

  if (length(region_inc_file[stringr::str_detect(region_inc_file, "set map_*")]) == 0) {
    return(NULL)
  }

  # find mapping name
  map_name <- stringr::str_sub(basename(f), 1, -5)
  print(map_name)

  # find descriptions
  set.begin <- grep(paste0("set","\\s+",map_name), tolower(region_inc_file))[1]
  set.end <- grep(";", region_inc_file[set.begin:length(region_inc_file)])[1]
  region_inc_desc <- region_inc_file[(set.begin + 1):(set.end - 1)]
  region_inc_desc <- region_inc_desc[region_inc_desc != ""]

  # Add NA if no Description
  with_desc <- grep("#", region_inc_desc)
  idx_no_desc <- !(1:length(region_inc_desc)) %in% with_desc
  region_inc_desc[idx_no_desc] = paste(region_inc_desc[idx_no_desc],"# NA")

  region_inc_desc <- stringr::str_split(region_inc_desc, "#")
  region_inc_desc <- data.table::data.table(matrix(unlist(region_inc_desc), ncol = 2, byrow = TRUE))
  region_inc_desc[, V1 := tolower(stringr::str_trim(V1))]
  region_inc_desc[, V2 := stringr::str_trim(V2)]
  data.table::setnames(region_inc_desc, c(map_name, "description"))

  return(region_inc_desc)
}

# Region descriptions
region_descriptions <- lapply(region_mapping_files, load_region_description)
names(region_descriptions) <- stringr::str_sub(basename(region_mapping_files), 1, -5)

usethis::use_data(region_descriptions, compress = "xz", overwrite = T)

