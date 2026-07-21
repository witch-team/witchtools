test_that("convert_gdx errors when the gdx file does not exist", {
  skip_if_not_installed("gdxtools", "1.0.0")

  outdir <- withr::local_tempdir()

  expect_error(
    convert_gdx(
      file.path(outdir, "does_not_exist.gdx"),
      "witch17", "t30", outdir,
      region_mappings = witchtools::region_mappings,
      time_mappings = witchtools::time_mappings
    ),
    "does not exist"
  )
})

test_that("convert_gdx writes a converted gdx and returns the input path", {
  skip_if_not_installed("gdxtools", "1.0.0")

  gdxfile <- withr::local_tempfile(pattern = "data_", fileext = ".gdx")
  outdir <- withr::local_tempdir()

  regs <- unique(region_mappings[["witch12"]][["witch12"]])
  param <- data.frame(
    n = regs,
    t = "1",
    value = as.numeric(seq_along(regs)),
    stringsAsFactors = FALSE
  )
  gdxtools::write.gdx(gdxfile, params = list(mypar = param))

  res <- suppressMessages(convert_gdx(gdxfile, "witch17", "t30", outdir,
    region_mappings = witchtools::region_mappings,
    time_mappings = witchtools::time_mappings,
    region_name = "n", guess_region = "witch12"
  ))

  expect_equal(res, gdxfile)

  outfile <- file.path(outdir, basename(gdxfile))
  expect_true(file.exists(outfile))

  .gdx <- gdxtools::gdx(outfile)
  expect_equal(.gdx$parameters$name, "mypar")

  out <- data.table::setDT(.gdx["mypar"])
  expect_equal(colnames(out), c("n", "t", "value"))
  expect_equal(
    sort(unique(out$n)),
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

  got <- out[t == "1", .(n, value)]

  data.table::setkey(got, n)
  data.table::setkey(expected, n)

  expect_equal(got, expected)
})

test_that("convert_gdx extrapolates over the full time mapping", {
  skip_if_not_installed("gdxtools", "1.0.0")

  gdxfile <- withr::local_tempfile(pattern = "data_", fileext = ".gdx")
  outdir <- withr::local_tempdir()

  regs <- unique(region_mappings[["witch12"]][["witch12"]])
  param <- data.frame(
    n = regs, t = "1", value = as.numeric(seq_along(regs)),
    stringsAsFactors = FALSE
  )
  gdxtools::write.gdx(gdxfile, params = list(mypar = param))

  suppressMessages(convert_gdx(gdxfile, "witch17", "t30", outdir,
    region_mappings = witchtools::region_mappings,
    time_mappings = witchtools::time_mappings,
    region_name = "n", guess_region = "witch12"
  ))

  out <- data.table::setDT(
    gdxtools::gdx(file.path(outdir, basename(gdxfile)))["mypar"]
  )

  nt <- length(unique(time_mappings[["t30"]][["t"]]))
  nreg <- length(unique(region_mappings[["witch17"]][["witch17"]]))

  expect_equal(nrow(out), nt * nreg)
  expect_equal(length(unique(out[n == "usa"]$value)), 1)
})

test_that("convert_gdx renames non-region, non-time indices to '*'", {
  skip_if_not_installed("gdxtools", "1.0.0")

  gdxfile <- withr::local_tempfile(pattern = "data_", fileext = ".gdx")
  outdir <- withr::local_tempdir()

  regs <- unique(region_mappings[["witch12"]][["witch12"]])
  param <- data.frame(
    n = rep(regs, 2),
    fuel = rep(c("oil", "gas"), each = length(regs)),
    t = "1",
    value = as.numeric(seq_len(2 * length(regs))),
    stringsAsFactors = FALSE
  )
  gdxtools::write.gdx(gdxfile, params = list(mypar = param))

  suppressMessages(convert_gdx(gdxfile, "witch17", "t30", outdir,
    region_mappings = witchtools::region_mappings,
    time_mappings = witchtools::time_mappings,
    region_name = "n", guess_region = "witch12"
  ))

  out <- data.table::setDT(
    gdxtools::gdx(file.path(outdir, basename(gdxfile)))["mypar"]
  )

  # 'n' and 't' keep their names, the free index is written as '*' and read
  # back with a positional name
  expect_true(all(c("n", "t", "value") %in% colnames(out)))
  expect_equal(ncol(out), 4)
  expect_equal(sort(unique(out[[setdiff(colnames(out), c("n", "t", "value"))]])),
    c("gas", "oil")
  )
})

test_that("convert_gdx honours default_meta_param", {
  skip_if_not_installed("gdxtools", "1.0.0")

  gdxfile <- withr::local_tempfile(pattern = "data_", fileext = ".gdx")
  outdir <- withr::local_tempdir()

  regs <- unique(region_mappings[["witch12"]][["witch12"]])
  param <- data.frame(
    n = regs, t = "1", value = as.numeric(seq_along(regs)),
    stringsAsFactors = FALSE
  )

  gdxtools::write.gdx(gdxfile, params = list(mypar = param))

  suppressMessages(convert_gdx(gdxfile, "witch17", "t30", outdir,
    region_mappings = witchtools::region_mappings,
    time_mappings = witchtools::time_mappings,
    region_name = "n", guess_region = "witch12",
    default_meta_param = data.table::data.table(
      parameter = "mypar", type = "extrap", value = "skip"
    )
  ))

  out <- data.table::setDT(
    gdxtools::gdx(file.path(outdir, basename(gdxfile)))["mypar"]
  )

  expect_equal(unique(out$t), "1")
})

test_that("convert_gdx errors when a requested weight is unknown", {
  skip_if_not_installed("gdxtools", "1.0.0")

  gdxfile <- withr::local_tempfile(pattern = "data_", fileext = ".gdx")
  outdir <- withr::local_tempdir()

  regs <- unique(region_mappings[["witch12"]][["witch12"]])
  param <- data.frame(
    n = regs, t = "1", value = as.numeric(seq_along(regs)),
    stringsAsFactors = FALSE
  )
  gdxtools::write.gdx(gdxfile, params = list(mypar = param))

  expect_error(
    suppressMessages(convert_gdx(gdxfile, "witch17", "t30", outdir,
      region_mappings = witchtools::region_mappings,
      time_mappings = witchtools::time_mappings,
      region_name = "n", guess_region = "witch12",
      default_meta_param = data.table::data.table(
        parameter = "mypar", type = "nweight", value = "not_a_weight"
      )
    )),
    "not_a_weight not in weights"
  )
})

test_that("convert_gdx works with its documented default mappings", {
  skip_if_not_installed("gdxtools", "1.0.0")

  gdxfile <- withr::local_tempfile(pattern = "data_", fileext = ".gdx")
  outdir <- withr::local_tempdir()

  regs <- unique(region_mappings[["witch12"]][["witch12"]])
  param <- data.frame(
    n = regs, t = "1", value = as.numeric(seq_along(regs)),
    stringsAsFactors = FALSE
  )
  gdxtools::write.gdx(gdxfile, params = list(mypar = param))

  # region_mappings and time_mappings are documented as optional, so calling
  # convert_gdx without them must work exactly as when they are passed.
  expect_no_error(
    suppressMessages(convert_gdx(gdxfile, "witch17", "t30", outdir,
      region_name = "n", guess_region = "witch12"
    ))
  )
})
