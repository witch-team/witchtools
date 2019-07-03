# Write additionnal GAMS include file

write_gams <- function(reg_id,
                       time_id,
                       region_mappings,
                       region_definitions,
                       time_mappings,
                       weights,
                       output_directory) {

  #write region conf
  filename = file.path(output_directory, 'regions.conf')
  fconf = file(filename, "w")

  #setglobal nmapping
  writeLines(paste("$setglobal nmapping", reg_id), fconf)

  close(fconf)

  ###############################################################################

  #write region mapping
  filename = file.path(output_directory, 'n.inc')
  finc = file(filename, "w")
  reg = sort(region_definitions[[reg_id]][, get(reg_id)])
  writeLines(reg, finc)
  close(finc)

  ###############################################################################

  #write region mapping for database report
  filename = file.path(output_directory, 'map_nrep_n.inc')
  finc = file(filename, "w")
  writeLines(paste(reg,reg,sep = "."), finc)
  writeLines(paste0("WORLD.(",paste(reg,collapse = ","),")"), finc)
  close(finc)

  ###############################################################################

  filename = file.path(output_directory, 'regions.inc')
  finc = file(filename, "w")
  #set iso3
  writeLines("set iso3 'Country definition in ISO_3166-1_alpha-3' /",finc)
  writeLines(sort(region_mappings[[reg_id]]$iso3), finc)
  writeLines("/;", finc)
  #set map_n_iso3
  writeLines("set map_n_iso3(n,iso3) 'Mapping between WITCH regions and countries/territories'/",finc)
  writeLines(region_mappings[[reg_id]][, paste(get(reg_id), iso3, sep = ".")], finc)
  writeLines("/;", finc)
  #set oecd
  writeLines("set oecd(n) 'OECD regions' /", finc)
  writeLines(oecd_regions(reg_id,
                          region_mappings,
                          weights), finc)
  writeLines("/;", finc)

  close(finc)


  ###############################################################################

  ttt = time_mappings[[time_id]][year == refyear]

  #write time mapping
  filename = file.path(output_directory, 'time.inc')
  finc = file(filename, "w")

  #set t
  writeLines("set t /", finc)
  writeLines(ttt[, paste(t)], finc)
  writeLines("/;", finc)

  #set pre
  writeLines("set pre(t,tp1) /", finc)
  writeLines(ttt[!is.na(pred) & (stringr::str_length(pred) > 0), paste(pred, t, sep = ".")], finc)
  writeLines("/;", finc)

  #param tperiod(t)
  writeLines(ttt[, paste0("tperiod('", t, "')=", tperiod, ";")], finc)

  #param year(t)
  writeLines(ttt[, paste0("year('", t, "')=", refyear, ";")], finc)

  #param tlen(t)
  writeLines(ttt[, paste0("tlen('", t, "')=", as.numeric(endyear) - as.numeric(begyear) + 1, ";")], finc)

  #param begyear(t)
  writeLines(ttt[, paste0("begyear('", t, "')=", as.numeric(begyear), ";")], finc)

  get_predecessors <- function(x){
    pred <- c()
    repeat {
      if (ttt[t == x]$pred == "") break
      x <- ttt[t == x]$pred
      pred = c(x,pred)
    }
    return(c(pred))
  }

  #set preds(t,tt)
  for (t in unique(ttt$t)) {
    preds <- get_predecessors(t)
    if (!is.null(preds)) {
      writeLines(paste0("preds('", t, "','", preds,"')=yes;"), finc)
    }
  }

  close(finc)

  ###############################################################################

  #write noncoop conf
  filename = file.path(output_directory, 'noncoop.conf')
  fconf = file(filename, "w")

  #setglobal nmapping
  writeLines(paste("$setglobal coalitions", paste(
    stringr::str_c("c_", region_definitions[[reg_id]][, get(reg_id)]), collapse = " "
  )), fconf)

  close(fconf)

  ###############################################################################

  #write noncoop inc
  filename = file.path(output_directory, 'noncoop.inc')
  finc = file(filename, "w")

  #set clt
  writeLines("set clt 'Coalitions' /", finc)
  writeLines(stringr::str_c("c_", region_definitions[[reg_id]][, get(reg_id)]), finc)
  writeLines("/;", finc)

  #set map_clt+n
  writeLines("set map_clt_n(clt,n) 'Mapping between all coalitions and regions' /",
             finc)
  writeLines(stringr::str_c("c_", region_definitions[[reg_id]][, get(reg_id)], ".", region_definitions[[reg_id]][, get(reg_id)]),
             finc)
  writeLines("/;", finc)

  close(finc)

}
