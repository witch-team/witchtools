# default time mappings

library(data.table)

# Timescale mappings
time_mapping_files <- Sys.glob(file.path("data-raw", "timescale", "*.csv"))
time_mappings <- lapply(time_mapping_files, witchtools::witch_time_mapping)
names(time_mappings) <- stringr::str_sub(basename(time_mapping_files), 1, -5)

usethis::use_data(time_mappings, compress = "xz", overwrite = T)
