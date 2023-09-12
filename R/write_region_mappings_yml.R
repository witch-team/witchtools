#' Write the IAMC regional mapping to common regions for WITCH yml file as in
#' https://github.com/IAMconsortium/common-definitions/blob/main/mappings/REMIND_3.1.yml
#'
#' \code{make_reg_mapping_yml} write a yaml file with the region mappings.

#' @param model model version
#' @param regdef  Witch region definition. THE REGION MUST BE DEFINED IN WTCHTOOLS.
#' @param filename yaml filename to write
#'
#' @author Lara Aleluia Reis
#'
#' @export
#'
write_region_mappings_yml <- function(model = 'WITCH 5.0', regdef = 'witch17',
                                      filename = NULL){

  # Get the region description match to the n WITCH regions
  datr = merge(region_descriptions[[regdef]],
               region_mappings[[regdef]],
               by = regdef)

  # Create concatenated column for the reporting format
  datr$descriptionmr = paste0(model,'|',datr$description)

  # Make the list structures (based on the common-definitions/definitions/region/native_regions/REMIND_3.1.yml)
  m <- unique(datr[[regdef]])
  ii <- NULL
  for (i in seq_along(m)) {
    oo = list(unique(datr[witch17 == m[i], descriptionmr])) # TODO use regdef
    names(oo) <- m[i]
    ii <- c(ii, oo)
  }

  creg <- list(
    World = unique(datr[[regdef]]),
    `Asia (R5)` = c('china','india','indonesia','sasia','seasia'),
    `Latin America (R5)` = c('brazil','mexico','laca'),
    `Middle East & Africa (R5)`=c('mena','southafrica','ssa'),
    `OECD & EU (R5)`=c('canada','europe','jpnkor','oceania','usa'),
    `Reforming Economies (R5)` = c('te'),
    `Africa (R10)`=c('ssa','southafrica'),
    `China+ (R10)`=c('china'),
    `Europe (R10)`=c('europe'),
    `India+ (R10)`=c('india'),
    `Latin America (R10)`=c('laca','brazil','mexico'),
    `Middle East (R10)`=c('mena'),
    `North America (R10)`=c('usa','canada'),
    `Reforming Economies (R10)`=c('te'),
    `Rest of Asia (R10)`=c('sasia','seasia','jpnkor'),
    `Pacific OECD (R10)`=c('oceaina'),
    `Other (R10)`=c('indonesia')
  )


  ragreg = list(list(model = list(model)),
                list(native_regions = ii),
                list(common_regions = creg))

   la <- yaml::as.yaml(ragreg)

  #write file
   if (is.null(filename)) {
     write(la, paste0(str_replace(model,' ','_'),'.yml'))
   } else {
     write(la, filename)
   }

   invisible(NULL)

}

