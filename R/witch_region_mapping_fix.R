# temporary fix to function witch_region_mapping in witchtools
witch_region_mapping <- function(f) {
  rif <- readLines(f)
  rif <- rif[rif != ""]
  rif <- rif[!stringr::str_detect(rif, "^\\*")]
  rif <- stringr::str_trim(stringr::str_split_fixed(rif, "#", 
                                                    2)[, 1])
  if (length(rif[stringr::str_detect(rif, "set map_*")]) == 
      0) {
    return(NULL)
  }
  set.begin <- grep("set map_*", tolower(rif))[1]
  set.end <- set.begin + grep(";", rif[set.begin:length(rif)])[1]
  rim <- rif[(set.begin + 1):(set.end - 2)]
  rim <- stringr::str_split(rim, "\\.")
  rim <- rim[rim != ""] #NEW!
  
  rim <- data.table::data.table(matrix(unlist(rim), ncol = 2, 
                                       byrow = TRUE))
  rim[, `:=`(V1, tolower(V1))]
  rim[, `:=`(V2, toupper(V2))]
  data.table::setnames(rim, c(stringr::str_sub(basename(f), 
                                               1, -5), "iso3"))
  return(rim)}
