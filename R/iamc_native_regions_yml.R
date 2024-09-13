#' Write the IAMC native regions in yaml as defined in
#' https://github.com/IAMconsortium/common-definitions
#'
#' \code{iamc_native_regions_yml} write a yaml file with the native regions.

#' @param filename yaml filename to write
#' @param model model version
#' @param n  Witch region definition, as defined in witchtools.
#'
#' @author Lara Aleluia Reis
#'
#' @export
#'
iamc_native_regions_yml <- function(filename = NULL,
                                     model = 'WITCH 5.0',
                                     n ='witch17'){

  # Get the region descrition match to the n WITCH regions
  datr = merge(witchtools::region_descriptions[[n]],
               witchtools::region_mappings[[n]],
               by = n)

  # Keep only ISO country code
  datr <- datr[iso3 %in% unique(countrycode::codelist$iso3c)]

  # Make the list structure
  m <- unique(datr$description)
  ii <- NULL
  for (i in seq_along(m)) {
    oo <- list(desp = list(iso3_codes = datr[description == m[i]]$iso3))
    names(oo) = paste(model, m[i], sep = '|')
    ii <- c(ii, list(oo))
  }

  # Add the primary ident structure
  x <- list(list(model = ii ))
  names(x[[1]]) = model

  la = yaml::as.yaml(x)

  #write file
  if (is.null(filename)) {
    write(la, paste0('native_regions_', stringr::str_replace_all(model,'[ .]','_'),'.yml'))
  } else {
    write(la, filename)
  }

  invisible(NULL)

}

