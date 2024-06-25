#' Write the IAMC regional mapping to common regions for WITCH yml file as in
#' https://github.com/IAMconsortium/common-definitions
#'
#' \code{write_region_mappings_yml} write a yaml file with the region mappings.

#' @param filename yaml filename to write
#' @param model model version
#' @param n  Witch region definition. THE REGION MUST BE DEFINED IN WTCHTOOLS.
#' @param comm_regs Additional mappings, which region aggregate, to add.
#'
#' @author Lara Aleluia Reis
#'
#' @export
#'
write_region_mappings_yml <- function(filename = NULL,
                                      model = 'WITCH 5.0',
                                      n = 'witch17',
                                      comm_regs = c('world','r5','r9','r10')){

  # Get the region description match to the n WITCH regions
  datr = merge(region_descriptions[[n]],
               region_mappings[[n]],
               by = n)

  # Keep only ISO country code
  datr <- datr[iso3 %in% unique(countrycode::codelist$iso3c)]

  # Create concatenated column for the reporting format
  datr$descriptionmr = paste0(model,'|',datr$description)

  # Make the list structures
  m <- unique(datr[[n]])
  ii <- NULL
  for (i in seq_along(m)) {
    oo = list(unique(datr[get(n) == m[i]]$descriptionmr))
    names(oo) <- m[i]
    ii <- c(ii, list(oo))
  }

  # Add common regions
  creg <- list()

  if ('world' %in% tolower(comm_regs)) {
    creg <- append(creg,
      list(list(World = as.list(unique(datr[[n]]))))
    )
  }

  for (cmap in comm_regs) {

    if (tolower(cmap) == 'world') {
      next
    }

    if (cmap %in% names(region_mappings)) {

      datm <- merge(region_descriptions[[cmap]],
                   region_mappings[[cmap]],
                   by = cmap)
      datm <- merge(datm,
                   datr[,.(reg=get(n),iso3)],
                   by = 'iso3')
      datm <- merge(datm,
                   default_weights[['gdp']],
                   by = 'iso3')

      # Choose aggregate region to highest share in economy
      datm <- datm[, .(ww = sum(weight)), by = c(cmap,'description','reg')]
      datm <- datm[, .SD[ww==max(ww),], by = c('reg')]


      for (i in sort(unique(datm$description))) {
        oo <- list(as.list(datm[description == i, sort(unique(reg))]))
        names(oo) <- i
        creg <- append(creg, list(oo))
      }


    } else {
      warning(paste(cmap,' is not defined in region_mappings.'))
    }

  }

  ragreg = list(model = list(model),
                native_regions = ii,
                common_regions = creg)

   la <- yaml::as.yaml(ragreg, indent.mapping.sequence = TRUE)

  #write file
   if (is.null(filename)) {
     write(la, paste0(stringr::str_replace(model,' ','_'),'.yml'))
   } else {
     write(la, filename)
   }

   invisible(NULL)

}

