test_that("convert_sqlite errors when the database does not exist", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  outdir <- withr::local_tempdir()

  expect_error(
    convert_sqlite(
      file.path(outdir, "does_not_exist.sqlite"),
      "witch17", "t30", outdir
    ),
    "does not exist"
  )
})

test_that("convert_sqlite writes a converted database with the same base name", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  dbfile <- withr::local_tempfile(pattern = "data_", fileext = ".sqlite")
  outdir <- withr::local_tempdir()

  regs <- unique(region_mappings[["witch12"]][["witch12"]])
  tab <- data.frame(
    n = regs,
    t = "1",
    value = as.numeric(seq_along(regs)),
    stringsAsFactors = FALSE
  )

  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = dbfile)
  RSQLite::dbWriteTable(con, "mypar", tab)
  RSQLite::dbDisconnect(con)

  suppressMessages(convert_sqlite(dbfile, "witch17", "t30", outdir,
    region_name = "n", guess_region = "witch12"
  ))

  outfile <- file.path(outdir, basename(dbfile))
  expect_true(file.exists(outfile))

  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = outfile)
  withr::defer(RSQLite::dbDisconnect(con))

  expect_equal(RSQLite::dbListTables(con), "mypar")

  res <- data.table::setDT(RSQLite::dbGetQuery(con, "select * from mypar"))

  expect_equal(colnames(res), c("n", "t", "value"))
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

test_that("convert_sqlite extrapolates over the full time mapping", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  dbfile <- withr::local_tempfile(pattern = "data_", fileext = ".sqlite")
  outdir <- withr::local_tempdir()

  regs <- unique(region_mappings[["witch12"]][["witch12"]])
  tab <- data.frame(
    n = regs, t = "1", value = as.numeric(seq_along(regs)),
    stringsAsFactors = FALSE
  )

  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = dbfile)
  RSQLite::dbWriteTable(con, "mypar", tab)
  RSQLite::dbDisconnect(con)

  suppressMessages(convert_sqlite(dbfile, "witch17", "t30", outdir,
    region_name = "n", guess_region = "witch12"
  ))

  con <- RSQLite::dbConnect(RSQLite::SQLite(),
    dbname = file.path(outdir, basename(dbfile))
  )
  withr::defer(RSQLite::dbDisconnect(con))

  res <- data.table::setDT(RSQLite::dbGetQuery(con, "select * from mypar"))

  nt <- length(unique(time_mappings[["t30"]][["t"]]))
  nreg <- length(unique(region_mappings[["witch17"]][["witch17"]]))

  expect_equal(nrow(res), nt * nreg)
  # constant extrapolation of a single input period
  expect_equal(length(unique(res[n == "usa"]$value)), 1)
})

test_that("convert_sqlite reads conversion options from the meta_param table", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  dbfile <- withr::local_tempfile(pattern = "data_", fileext = ".sqlite")
  outdir <- withr::local_tempdir()

  regs <- unique(region_mappings[["witch12"]][["witch12"]])
  tab <- data.frame(
    n = regs, t = "1", value = as.numeric(seq_along(regs)),
    stringsAsFactors = FALSE
  )
  meta <- data.frame(
    parameter = "mypar", type = "nagg", value = "sumby",
    stringsAsFactors = FALSE
  )

  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = dbfile)
  RSQLite::dbWriteTable(con, "mypar", tab)
  RSQLite::dbWriteTable(con, "meta_param", meta)
  RSQLite::dbDisconnect(con)

  suppressMessages(convert_sqlite(dbfile, "witch17", "t30", outdir,
    region_name = "n", guess_region = "witch12"
  ))

  con <- RSQLite::dbConnect(RSQLite::SQLite(),
    dbname = file.path(outdir, basename(dbfile))
  )
  withr::defer(RSQLite::dbDisconnect(con))

  # 'sumby' produces an extra coverage table, and meta_param is not converted
  expect_equal(sort(RSQLite::dbListTables(con)), c("mypar", "mypar_info"))

  info <- data.table::setDT(
    RSQLite::dbGetQuery(con, "select * from mypar_info")
  )
  expect_equal(colnames(info), c("n", "t", "value"))
  expect_true(all(info$value >= 0 & info$value <= 1 + 1e-9))
})

test_that("convert_sqlite accepts a default_meta_param", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  dbfile <- withr::local_tempfile(pattern = "data_", fileext = ".sqlite")
  outdir <- withr::local_tempdir()

  regs <- unique(region_mappings[["witch12"]][["witch12"]])
  tab <- data.frame(
    n = regs, t = "1", value = as.numeric(seq_along(regs)),
    stringsAsFactors = FALSE
  )

  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = dbfile)
  RSQLite::dbWriteTable(con, "mypar", tab)
  RSQLite::dbDisconnect(con)

  suppressMessages(convert_sqlite(dbfile, "witch17", "t30", outdir,
    region_name = "n", guess_region = "witch12",
    default_meta_param = data.table::data.table(
      parameter = "mypar", type = "extrap", value = "skip"
    )
  ))

  con <- RSQLite::dbConnect(RSQLite::SQLite(),
    dbname = file.path(outdir, basename(dbfile))
  )
  withr::defer(RSQLite::dbDisconnect(con))

  res <- data.table::setDT(RSQLite::dbGetQuery(con, "select * from mypar"))

  # extrapolation skipped: only the input period is kept
  expect_equal(unique(res$t), "1")
})

test_that("convert_sqlite errors when a requested weight is unknown", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  dbfile <- withr::local_tempfile(pattern = "data_", fileext = ".sqlite")
  outdir <- withr::local_tempdir()

  regs <- unique(region_mappings[["witch12"]][["witch12"]])
  tab <- data.frame(
    n = regs, t = "1", value = as.numeric(seq_along(regs)),
    stringsAsFactors = FALSE
  )
  meta <- data.frame(
    parameter = "mypar", type = "nweight", value = "not_a_weight",
    stringsAsFactors = FALSE
  )

  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = dbfile)
  RSQLite::dbWriteTable(con, "mypar", tab)
  RSQLite::dbWriteTable(con, "meta_param", meta)
  RSQLite::dbDisconnect(con)

  expect_error(
    suppressMessages(convert_sqlite(dbfile, "witch17", "t30", outdir,
      region_name = "n", guess_region = "witch12"
    )),
    "not_a_weight not in weights"
  )
})
