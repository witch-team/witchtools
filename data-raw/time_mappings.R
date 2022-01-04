# default time mappings

library(data.table)

load_timescale_mapping <- function(f) {
  tab <- data.table::fread(f, colClasses = "character")
  data.table::setnames(tab, "year", "refyear")
  # Expand year
  tab <- rbindlist(lapply(
    seq_len(nrow(tab)),
    function(i) {
      tab[i, .(t,
        year = begyear:endyear,
        refyear,
        pred,
        tperiod,
        begyear,
        endyear
      )]
    }
  ))
  tab[, year := as.numeric(year)]
  return(tab)
}

# Timescale mappings
time_mapping_files <- Sys.glob(file.path("data-raw", "timescale", "*.csv"))
time_mappings <- lapply(time_mapping_files, witchtools::witch_time_mapping)
names(time_mappings) <- stringr::str_sub(basename(time_mapping_files), 1, -5)

usethis::use_data(time_mappings, compress = "xz", overwrite = T)
