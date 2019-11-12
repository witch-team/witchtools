#' Load region definition from a regional WITCH GAMS file.
#'
#' @param f Regional WITCH GAMS file
#'
#' @export
#' @import data.table
#' @examples
#' \dontrun{
#' load_region_definition('input/regions/witch17.inc')
#' }
#'
load_region_definition = function(f){

  region_inc_file <- readLines(f)
  region_inc_file = region_inc_file[region_inc_file != ""]                    # Remove empty lines
  region_inc_file = region_inc_file[!stringr::str_detect(region_inc_file,"^\\*")]    # Remove * comments
  region_inc_file = stringr::str_trim(stringr::str_split_fixed(region_inc_file,"#",2)[,1])    # Remove # comments

  set.begin = grep("set*",tolower(region_inc_file))[1]
  set.end = set.begin + grep(";",region_inc_file[set.begin:length(region_inc_file)])[1]
  region_inc = region_inc_file[(set.begin + 1):(set.end - 2)]
  region_inc = gsub("\ *'.*'", "", region_inc)                              # clean names, remove comments
  region_inc = gsub("\\t", "", region_inc)
  region_inc = region_inc[stringr::str_length(region_inc) > 0]
  region_inc <- data.table(matrix(unlist(region_inc), ncol = 1, byrow = TRUE))
  if (!stringr::str_detect(f,"iso3")) {region_inc[,V1 := tolower(V1)]}

  setnames(region_inc,c(stringr::str_sub(basename(f), 1, -5)))

  return(region_inc)

}

#' Load region mapping from a regional WITCH GAMS file.
#'
#' @param f Regional WITCH GAMS file
#'
#' @export
#' @import data.table
#' @examples
#' \dontrun{
#' load_region_mapping('input/regions/witch17.inc')
#' }
#'
load_region_mapping = function(f){

  region_inc_file <- readLines(f)
  region_inc_file = region_inc_file[region_inc_file != ""]                    # Remove empty lines
  region_inc_file = region_inc_file[!stringr::str_detect(region_inc_file,"^\\*")]    # Remove * comments
  region_inc_file = stringr::str_trim(stringr::str_split_fixed(region_inc_file,"#",2)[,1])    # Remove # comments

  if (length(region_inc_file[stringr::str_detect(region_inc_file,"set map_*")]) == 0) return(NULL)

  set.begin = grep("set map_*",tolower(region_inc_file))[1]
  set.end = set.begin + grep(";",region_inc_file[set.begin:length(region_inc_file)])[1]
  region_inc_map = region_inc_file[(set.begin + 1):(set.end - 2)]
  region_inc_map = stringr::str_split(region_inc_map,"\\.")
  region_inc_map <- data.table(matrix(unlist(region_inc_map), ncol = 2, byrow = TRUE))
  region_inc_map[,V1 := tolower(V1)]
  region_inc_map[,V2 := toupper(V2)]
  setnames(region_inc_map,c(stringr::str_sub(basename(f), 1, -5), "iso3"))

  return(region_inc_map)

}
