#' Write the IAMC native regions in yaml as defined in
#' https://github.com/IAMconsortium/common-definitions
#'
#' \code{write_native_regions_yml} write a yaml file with the native regions.

#' @param model model version
#' @param regdef  Witch region definition. THE REGION MUST BE DEFINED IN WTCHTOOLS.
#' @param filename yaml filename to write
#'
#' @author Lara Aleluia Reis
#'
#' @export
#'
write_native_regions_yml <- function(model = 'WITCH 5.0', regdef ='witch17',
                                     filename = NULL){

  # Get the region descrition match to the n WITCH regions
  datr = merge(region_descriptions[[regdef]],
               region_mappings[[regdef]],
               by = regdef)

  # Make the list structure
  m <- unique(datr$description)
  ii <- NULL
  for (i in seq_along(m)) {
    oo <- list(desp = list(iso3_codes = datr[description == m[i]]$iso3))
    names(oo) = paste(model, m[i], sep = '|')
    ii <- c(ii, oo)
  }

  # Add the primary ident structure
  x <- list(list(model = ii ))
  names(x[[1]]) = model

  la = yaml::as.yaml(x)

  #write file
  if (is.null(filename)) {
    write(la, paste0(str_replace(model,' ','_'),'.yml'))
  } else {
    write(la, filename)
  }

  invisible(NULL)

}

