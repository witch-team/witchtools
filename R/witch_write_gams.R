# Write additional GAMS include file
# Not exported, called by witch_translate_data
witch_write_gams <- function(region_mapping,
                             time_mapping,
                             output_directory,
                             prefix_coalition = "c_") {
  reg_id <- region_id(region_mapping)

  # write region conf
  filename <- file.path(output_directory, "regions.conf")
  fconf <- file(filename, "w")

  # setglobal nmapping
  writeLines(paste("$setglobal nmapping", reg_id), fconf)

  close(fconf)

  ##############################################################################

  # write region mapping
  filename <- file.path(output_directory, "n.inc")
  finc <- file(filename, "w")
  reg <- sort(unique(region_mapping[, get(reg_id)]))
  writeLines(reg, finc)
  close(finc)

  ##############################################################################

  # write region mapping for database report
  filename <- file.path(output_directory, "map_nrep_n.inc")
  finc <- file(filename, "w")
  writeLines(paste(reg, reg, sep = "."), finc)
  writeLines(paste0("WORLD.(", paste(reg, collapse = ","), ")"), finc)
  close(finc)

  ##############################################################################

  filename <- file.path(output_directory, "regions.inc")
  finc <- file(filename, "w")
  # set iso3
  writeLines("set iso3 'Country definition in ISO_3166-1_alpha-3' /", finc)
  writeLines(sort(region_mapping$iso3), finc)
  writeLines("/;", finc)
  # set map_n_iso3
  writeLines(
    "set map_n_iso3(n,iso3) 'Mapping between WITCH regions and iso3'/",
    finc
  )
  writeLines(
    region_mapping[, paste(get(reg_id), iso3, sep = ".")],
    finc
  )
  writeLines("/;", finc)
  # set oecd
  writeLines("set oecd(n) 'OECD regions' /", finc)
  writeLines(oecd_regions(region_mapping), finc)
  writeLines("/;", finc)
  # set eu
  writeLines("set eu(n) 'EU regions' /", finc)
  writeLines(eu_regions(region_mapping), finc)
  writeLines("/;", finc)
  # set eu27
  writeLines("set eu27(n) 'EU27 regions' /", finc)
  writeLines(eu27_regions(region_mapping), finc)
  writeLines("/;", finc)
  # set eu28
  writeLines("set eu28(n) 'EU28 regions' /", finc)
  writeLines(eu28_regions(region_mapping), finc)
  writeLines("/;", finc)
  # set is_europe
  writeLines("set is_europe(n) 'Europe regions' /", finc)
  writeLines(europe_regions(region_mapping), finc)
  writeLines("/;", finc)
  # set is_china
  writeLines("set is_china(n) 'China regions' /", finc)
  writeLines(china_regions(region_mapping), finc)
  writeLines("/;", finc)
  # set is_india
  writeLines("set is_india(n) 'India regions' /", finc)
  writeLines(india_regions(region_mapping), finc)
  writeLines("/;", finc)
  # set is_brazil
  writeLines("set is_brazil(n) 'Brazil regions' /", finc)
  writeLines(brazil_regions(region_mapping), finc)
  writeLines("/;", finc)
  # set is_usa
  writeLines("set is_usa(n) 'USA regions' /", finc)
  writeLines(usa_regions(region_mapping), finc)
  writeLines("/;", finc)
  # set is_oceania
  writeLines("set is_oceania(n) 'Oceania regions' /", finc)
  writeLines(oceania_regions(region_mapping), finc)
  writeLines("/;", finc)
  # set is_indonesia
  writeLines("set is_indonesia(n) 'Indonesia regions' /", finc)
  writeLines(indonesia_regions(region_mapping), finc)
  writeLines("/;", finc)
  # set is_ssa
  writeLines("set is_ssa(n) 'Sub-Saharan Africa regions' /", finc)
  writeLines(ssa_regions(region_mapping), finc)
  writeLines("/;", finc)

  close(finc)


  ##############################################################################

  ttt <- time_mappings[[time_id]][year == refyear]

  # write time mapping
  filename <- file.path(output_directory, "time.inc")
  finc <- file(filename, "w")

  # set t
  writeLines("set t /", finc)
  writeLines(ttt[, paste(t)], finc)
  writeLines("/;", finc)

  # set pre
  writeLines("set pre(t,tp1) /", finc)
  writeLines(ttt[
    !is.na(pred) & (stringr::str_length(pred) > 0),
    paste(pred, t, sep = ".")
  ], finc)
  writeLines("/;", finc)

  # param tperiod(t)
  writeLines(ttt[, paste0("tperiod('", t, "')=", tperiod, ";")], finc)

  # param year(t)
  writeLines(ttt[, paste0("year('", t, "')=", refyear, ";")], finc)

  # param tlen(t)
  writeLines(
    ttt[, paste0(
      "tlen('", t, "')=",
      as.numeric(endyear) - as.numeric(begyear) + 1, ";"
    )],
    finc
  )

  # param begyear(t)
  writeLines(
    ttt[, paste0("begyear('", t, "')=", as.numeric(begyear), ";")],
    finc
  )

  get_predecessors <- function(x) {
    pred <- c()
    repeat {
      if (ttt[t == x]$pred == "") break
      x <- ttt[t == x]$pred
      pred <- c(x, pred)
    }
    return(c(pred))
  }

  # set preds(t,tt)
  for (t in unique(ttt$t)) {
    preds <- get_predecessors(t)
    if (!is.null(preds)) {
      writeLines(paste0("preds('", t, "','", preds, "')=yes;"), finc)
    }
  }

  close(finc)

  ##############################################################################

  # write noncoop conf
  filename <- file.path(output_directory, "noncoop.conf")
  fconf <- file(filename, "w")

  # setglobal nmapping
  reg_def <- sort(unique(region_mapping[, get(reg_id)]))
  writeLines(paste("$setglobal coalitions", paste(
    stringr::str_c(prefix_coalition, reg_def),
    collapse = " "
  )), fconf)

  close(fconf)

  ##############################################################################

  # write noncoop2 conf
  filename <- file.path(output_directory, "noncoop2.conf")
  fconf <- file(filename, "w")

  # setglobal nmapping
  reg_def <- sort(unique(region_mapping[, get(reg_id)]))
  midpoint <- round(length(reg_def) / 2)
  reg_def1 <- reg_def[1:midpoint]
  reg_def2 <- reg_def[(midpoint+1):length(reg_def)]
  writeLines(paste("$setglobal coalition1", paste(
    stringr::str_c(prefix_coalition, reg_def1),
    collapse = " "
  )), fconf)
  writeLines(paste("$setglobal coalition2", paste(
    stringr::str_c(prefix_coalition, reg_def2),
    collapse = " "
  )), fconf)

  close(fconf)

  ##############################################################################

  # write noncoop inc
  filename <- file.path(output_directory, "noncoop.inc")
  finc <- file(filename, "w")

  # set clt
  writeLines("set clt 'Coalitions' /", finc)
  reg_def <- sort(unique(region_mapping[, get(reg_id)]))
  writeLines(stringr::str_c(prefix_coalition, reg_def), finc)
  writeLines("/;", finc)

  # set map_clt+n
  writeLines("set map_clt_n(clt,n) /", finc)
  writeLines(stringr::str_c(prefix_coalition, reg_def, ".", reg_def), finc)
  writeLines("/;", finc)

  close(finc)

}
