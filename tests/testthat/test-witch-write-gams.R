
test_that("witch_write_gams writes all expected GAMS include files", {
  out <- withr::local_tempdir()

  witch_write_gams(witchtools::region_mappings[["witch17"]],
                   witchtools::time_mappings[["t30"]], out)

  expect_setequal(
    basename(list.files(out)),
    c("regions.conf", "n.inc", "map_nrep_n.inc", "regions.inc", "time.inc",
      "noncoop.conf", "noncoop2.conf", "noncoop.inc")
  )
})

test_that("witch_write_gams writes the region id and the region list", {
  out <- withr::local_tempdir()
  rm <- witchtools::region_mappings[["witch17"]]
  reg <- sort(unique(rm[["witch17"]]))

  witch_write_gams(rm, witchtools::time_mappings[["t30"]], out)

  expect_equal(readLines(file.path(out, "regions.conf")),
               "$setglobal nmapping witch17")
  expect_equal(readLines(file.path(out, "n.inc")), reg)

  nrep <- readLines(file.path(out, "map_nrep_n.inc"))
  expect_equal(nrep[seq_along(reg)], paste(reg, reg, sep = "."))
  expect_equal(nrep[length(nrep)],
               paste0("WORLD.(", paste(reg, collapse = ","), ")"))
})

test_that("witch_write_gams writes regions.inc with the iso3 and region sets", {
  out <- withr::local_tempdir()
  rm <- witchtools::region_mappings[["witch17"]]

  witch_write_gams(rm, witchtools::time_mappings[["t30"]], out)
  lines <- readLines(file.path(out, "regions.inc"))

  expect_true(any(grepl("^set iso3 ", lines)))
  expect_true(all(sort(rm$iso3) %in% lines))
  expect_true(all(rm[, paste(witch17, iso3, sep = ".")] %in% lines))

  # every declared set is closed
  n_open <- sum(grepl("^set .*/$", lines))
  expect_equal(n_open, sum(lines == "/;"))
  expect_equal(n_open, 17L)

  # a few membership checks against the region_sets helpers
  block <- function(header) {
    i <- which(lines == header)
    j <- which(lines == "/;" & seq_along(lines) > i)[1]
    lines[(i + 1):(j - 1)]
  }
  expect_setequal(block("set is_usa(n) 'USA regions' /"), usa_regions(rm))
  expect_setequal(block("set eu27(n) 'EU27 regions' /"), eu27_regions(rm))
  expect_setequal(block("set is_japan(n) 'Japan regions' /"),
                  japan_regions(rm))
})

test_that("witch_write_gams writes the coalition config and include", {
  out <- withr::local_tempdir()
  rm <- witchtools::region_mappings[["witch17"]]
  reg <- sort(unique(rm[["witch17"]]))

  witch_write_gams(rm, witchtools::time_mappings[["t30"]], out)

  expect_equal(
    readLines(file.path(out, "noncoop.conf")),
    paste("$setglobal coalitions", paste(paste0("c_", reg), collapse = " "))
  )

  coal2 <- readLines(file.path(out, "noncoop2.conf"))
  expect_length(coal2, 2L)
  expect_match(coal2[1], "^\\$setglobal coalition1 ")
  expect_match(coal2[2], "^\\$setglobal coalition2 ")
  # the two coalitions partition the regions
  parts <- unlist(strsplit(sub("^\\$setglobal coalition[12] ", "", coal2), " "))
  expect_setequal(parts, paste0("c_", reg))

  inc <- readLines(file.path(out, "noncoop.inc"))
  expect_true(all(paste0("c_", reg) %in% inc))
  expect_true(all(paste0("c_", reg, ".", reg) %in% inc))
})

test_that("witch_write_gams honours prefix_coalition", {
  out <- withr::local_tempdir()
  rm <- witchtools::region_mappings[["witch17"]]

  witch_write_gams(rm, witchtools::time_mappings[["t30"]], out,
                   prefix_coalition = "coal_")

  expect_match(readLines(file.path(out, "noncoop.conf")), "coal_brazil")
  expect_true("coal_usa.usa" %in% readLines(file.path(out, "noncoop.inc")))
})

test_that("witch_write_gams works for another region mapping", {
  out <- withr::local_tempdir()
  rm <- witchtools::region_mappings[["r5"]]
  reg <- sort(unique(rm[["r5"]]))

  witch_write_gams(rm, witchtools::time_mappings[["t30"]], out)

  expect_equal(readLines(file.path(out, "regions.conf")),
               "$setglobal nmapping r5")
  expect_equal(readLines(file.path(out, "n.inc")), reg)
})

test_that("witch_write_gams writes time.inc consistently with the mapping", {
  out <- withr::local_tempdir()
  tm <- witchtools::time_mappings[["t30"]]
  ttt <- tm[year == refyear]

  witch_write_gams(witchtools::region_mappings[["witch17"]], tm, out)
  lines <- readLines(file.path(out, "time.inc"))

  expect_equal(lines[1], "set t /")
  i <- which(lines == "/;")[1]
  expect_equal(lines[2:(i - 1)], as.character(ttt$t))

  expect_true(all(ttt[, paste0("tperiod('", t, "')=", tperiod, ";")] %in% lines))
  expect_true(all(ttt[, paste0("year('", t, "')=", refyear, ";")] %in% lines))
  expect_true(all(
    ttt[, paste0("begyear('", t, "')=", as.numeric(begyear), ";")] %in% lines
  ))
  # first period has no predecessor, last one has all the others
  expect_false(any(grepl("^preds\\('1',", lines)))
  last <- ttt$t[nrow(ttt)]
  expect_equal(sum(grepl(paste0("^preds\\('", last, "',"), lines)),
               nrow(ttt) - 1L)
})

test_that("witch_write_gams uses the time_mapping it is given", {
  out <- withr::local_tempdir()

  # the output must follow the time_mapping argument
  tm <- witchtools::time_mappings[["t40"]]
  witch_write_gams(witchtools::region_mappings[["witch17"]], tm, out)

  lines <- readLines(file.path(out, "time.inc"))
  i <- which(lines == "/;")[1]
  expect_equal(lines[2:(i - 1)], as.character(tm[year == refyear]$t))
})

test_that("witch_write_gams errors when the output directory does not exist", {
  out <- file.path(withr::local_tempdir(), "nowhere")

  suppressWarnings(
    expect_error(
      witch_write_gams(witchtools::region_mappings[["witch17"]],
                       witchtools::time_mappings[["t30"]], out)
    )
  )
})
