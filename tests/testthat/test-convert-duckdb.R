test_that("convert_duckdb errors when the database does not exist", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")

  outdir <- withr::local_tempdir()

  expect_error(
    convert_duckdb(
      file.path(outdir, "does_not_exist.duckdb"),
      "witch17", "t30", outdir
    ),
    "does not exist"
  )
})

test_that("convert_duckdb writes a converted database with the same base name", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")

  dbdir <- withr::local_tempdir()
  outdir <- withr::local_tempdir()
  dbfile <- file.path(dbdir, "data_test.duckdb")

  regs <- unique(region_mappings[["witch12"]][["witch12"]])
  tab <- data.frame(
    n = regs,
    t = "1",
    value = as.numeric(seq_along(regs)),
    stringsAsFactors = FALSE
  )

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = dbfile)
  DBI::dbWriteTable(con, "mypar", tab)
  DBI::dbDisconnect(con, shutdown = TRUE)

  suppressMessages(convert_duckdb(dbfile, "witch17", "t30", outdir,
    region_name = "n", guess_region = "witch12"
  ))

  outfile <- file.path(outdir, basename(dbfile))
  expect_true(file.exists(outfile))

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = outfile, read_only = TRUE)
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))

  expect_equal(DBI::dbListTables(con), "mypar")

  res <- data.table::setDT(DBI::dbGetQuery(con, "select * from mypar"))

  expect_equal(sort(colnames(res)), sort(c("n", "t", "value")))
  expect_equal(
    sort(unique(res$n)),
    sort(unique(region_mappings[["witch17"]][["witch17"]]))
  )

  # Independent computation of the witch12 -> witch17 remapping for t == 1:
  # downscale to iso3 with the gdp weight, then sum by witch17
  mm <- merge(region_mappings[["witch12"]], region_mappings[["witch17"]],
    by = "iso3"
  )
  mm <- merge(mm, data.table::copy(default_weights[["gdp"]]), by = "iso3")
  mm <- merge(mm,
    data.table::data.table(
      witch12 = regs, value = as.numeric(seq_along(regs))
    ),
    by = "witch12"
  )
  mm[, tot := sum(weight), by = "witch12"]
  expected <- mm[, .(value = sum(value * weight / tot)), by = "witch17"]
  data.table::setnames(expected, "witch17", "n")

  got <- res[t == "1", .(n, value)]

  data.table::setkey(got, n)
  data.table::setkey(expected, n)

  expect_equal(got, expected)
})

test_that("convert_duckdb extrapolates over the full time mapping", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")

  dbdir <- withr::local_tempdir()
  outdir <- withr::local_tempdir()
  dbfile <- file.path(dbdir, "data_test.duckdb")

  regs <- unique(region_mappings[["witch12"]][["witch12"]])
  tab <- data.frame(
    n = regs, t = "1", value = as.numeric(seq_along(regs)),
    stringsAsFactors = FALSE
  )

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = dbfile)
  DBI::dbWriteTable(con, "mypar", tab)
  DBI::dbDisconnect(con, shutdown = TRUE)

  suppressMessages(convert_duckdb(dbfile, "witch17", "t30", outdir,
    region_name = "n", guess_region = "witch12"
  ))

  con <- DBI::dbConnect(duckdb::duckdb(),
    dbdir = file.path(outdir, basename(dbfile)), read_only = TRUE
  )
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))

  res <- data.table::setDT(DBI::dbGetQuery(con, "select * from mypar"))

  nt <- length(unique(time_mappings[["t30"]][["t"]]))
  nreg <- length(unique(region_mappings[["witch17"]][["witch17"]]))

  expect_equal(nrow(res), nt * nreg)
  expect_equal(length(unique(res[n == "usa"]$value)), 1)
})

test_that("convert_duckdb reads conversion options from the meta_param table", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")

  dbdir <- withr::local_tempdir()
  outdir <- withr::local_tempdir()
  dbfile <- file.path(dbdir, "data_test.duckdb")

  regs <- unique(region_mappings[["witch12"]][["witch12"]])
  tab <- data.frame(
    n = regs, t = "1", value = as.numeric(seq_along(regs)),
    stringsAsFactors = FALSE
  )
  meta <- data.frame(
    parameter = "mypar", type = "nagg", value = "sumby",
    stringsAsFactors = FALSE
  )

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = dbfile)
  DBI::dbWriteTable(con, "mypar", tab)
  DBI::dbWriteTable(con, "meta_param", meta)
  DBI::dbDisconnect(con, shutdown = TRUE)

  suppressMessages(convert_duckdb(dbfile, "witch17", "t30", outdir,
    region_name = "n", guess_region = "witch12"
  ))

  con <- DBI::dbConnect(duckdb::duckdb(),
    dbdir = file.path(outdir, basename(dbfile)), read_only = TRUE
  )
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))

  # 'sumby' produces an extra coverage table, and meta_param is not converted
  expect_equal(sort(DBI::dbListTables(con)), c("mypar", "mypar_info"))

  info <- data.table::setDT(
    DBI::dbGetQuery(con, "select * from mypar_info")
  )
  expect_equal(sort(colnames(info)), sort(c("n", "t", "value")))
  expect_true(all(info$value >= 0 & info$value <= 1 + 1e-9))
})

test_that("convert_duckdb accepts a default_meta_param", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")

  dbdir <- withr::local_tempdir()
  outdir <- withr::local_tempdir()
  dbfile <- file.path(dbdir, "data_test.duckdb")

  regs <- unique(region_mappings[["witch12"]][["witch12"]])
  tab <- data.frame(
    n = regs, t = "1", value = as.numeric(seq_along(regs)),
    stringsAsFactors = FALSE
  )

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = dbfile)
  DBI::dbWriteTable(con, "mypar", tab)
  DBI::dbDisconnect(con, shutdown = TRUE)

  suppressMessages(convert_duckdb(dbfile, "witch17", "t30", outdir,
    region_name = "n", guess_region = "witch12",
    default_meta_param = data.table::data.table(
      parameter = "mypar", type = "extrap", value = "skip"
    )
  ))

  con <- DBI::dbConnect(duckdb::duckdb(),
    dbdir = file.path(outdir, basename(dbfile)), read_only = TRUE
  )
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))

  res <- data.table::setDT(DBI::dbGetQuery(con, "select * from mypar"))

  # extrapolation skipped: only the input period is kept
  expect_equal(unique(res$t), "1")
})

test_that("convert_duckdb errors when a requested weight is unknown", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")

  dbdir <- withr::local_tempdir()
  outdir <- withr::local_tempdir()
  dbfile <- file.path(dbdir, "data_test.duckdb")

  regs <- unique(region_mappings[["witch12"]][["witch12"]])
  tab <- data.frame(
    n = regs, t = "1", value = as.numeric(seq_along(regs)),
    stringsAsFactors = FALSE
  )
  meta <- data.frame(
    parameter = "mypar", type = "nweight", value = "not_a_weight",
    stringsAsFactors = FALSE
  )

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = dbfile)
  DBI::dbWriteTable(con, "mypar", tab)
  DBI::dbWriteTable(con, "meta_param", meta)
  DBI::dbDisconnect(con, shutdown = TRUE)

  expect_error(
    suppressMessages(convert_duckdb(dbfile, "witch17", "t30", outdir,
      region_name = "n", guess_region = "witch12"
    )),
    "not_a_weight not in weights"
  )
})
