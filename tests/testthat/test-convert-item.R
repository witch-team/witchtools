test_that("convert_item aggregates regions and matches a manual computation", {
  regs <- unique(region_mappings[["witch12"]][["witch12"]])
  .data <- data.table::data.table(
    witch12 = regs,
    year = 2005,
    value = as.numeric(seq_along(regs))
  )

  res <- suppressMessages(convert_item(
    .data,
    reg_id = "witch17",
    time_id = "t30",
    region_name = "n",
    item_param = data.table::data.table(
      parameter = character(), type = character(), value = character()
    ),
    time_mappings = witchtools::time_mappings,
    region_mappings = witchtools::region_mappings,
    weights = witchtools::default_weights,
    guess_region = "witch17",
    guess_input_t = "t30",
    default_agg_missing = "NA"
  ))

  out <- res[[1]]

  expect_true(data.table::is.data.table(out))
  # the region column is renamed to region_name and 'year' becomes 't'
  expect_equal(colnames(out), c("n", "t", "value"))
  # sum aggregation produces no coverage info
  expect_null(res[[2]])

  # Independent computation of the witch12 -> witch17 remapping:
  # downscale to iso3 with the gdp weight, then sum by witch17
  w <- data.table::copy(witchtools::default_weights[["gdp"]])
  mm <- merge(region_mappings[["witch12"]], region_mappings[["witch17"]],
    by = "iso3"
  )
  mm <- merge(mm, w, by = "iso3")
  mm <- merge(mm,
    data.table::data.table(
      witch12 = regs,
      value = as.numeric(seq_along(regs))
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

test_that("convert_item extrapolates over the whole time mapping by default", {
  regs <- unique(region_mappings[["witch12"]][["witch12"]])
  .data <- data.table::data.table(
    witch12 = regs,
    year = 2005,
    value = as.numeric(seq_along(regs))
  )

  res <- suppressMessages(convert_item(
    .data, "witch17", "t30", "n", data.table::data.table(
      parameter = character(), type = character(), value = character()
    ),
    witchtools::time_mappings, witchtools::region_mappings,
    witchtools::default_weights, "witch17", "t30", "NA"
  ))
  out <- res[[1]]

  nt <- length(unique(time_mappings[["t30"]][["t"]]))
  nreg <- length(unique(region_mappings[["witch17"]][["witch17"]]))

  expect_equal(nrow(out), nt * nreg)
  # constant extrapolation: every period equals the 2005 value
  expect_equal(
    out[n == "usa"][order(as.numeric(t))]$value,
    rep(out[n == "usa" & t == "1"]$value, nt)
  )
})

test_that("convert_item honours the 'extrap' = 'skip' item parameter", {
  regs <- unique(region_mappings[["witch12"]][["witch12"]])
  .data <- data.table::data.table(
    witch12 = regs,
    year = 2005,
    value = as.numeric(seq_along(regs))
  )

  item_param <- data.table::data.table(
    parameter = "mypar", type = "extrap", value = "skip"
  )

  res <- suppressMessages(convert_item(
    .data, "witch17", "t30", "n", item_param,
    witchtools::time_mappings, witchtools::region_mappings,
    witchtools::default_weights, "witch17", "t30", "NA"
  ))
  out <- res[[1]]

  # no extrapolation: only the first period survives
  expect_equal(unique(out$t), "1")
  expect_equal(
    nrow(out),
    length(unique(region_mappings[["witch17"]][["witch17"]]))
  )
})

test_that("convert_item renames region_name to the guessed input mapping", {
  regs <- unique(region_mappings[["witch12"]][["witch12"]])
  .data <- data.table::data.table(
    n = regs,
    year = 2005,
    value = as.numeric(seq_along(regs))
  )

  res <- suppressMessages(convert_item(
    .data, "witch17", "t30", "n", data.table::data.table(
      parameter = character(), type = character(), value = character()
    ),
    witchtools::time_mappings, witchtools::region_mappings,
    witchtools::default_weights,
    guess_region = "witch12", guess_input_t = "t30",
    default_agg_missing = "NA"
  ))
  out <- res[[1]]

  expect_equal(colnames(out), c("n", "t", "value"))
  expect_equal(
    sort(unique(out$n)),
    sort(unique(region_mappings[["witch17"]][["witch17"]]))
  )
})

test_that("convert_item converts a 't' column into years when guess_input_t is t30", {
  regs <- unique(region_mappings[["witch12"]][["witch12"]])
  .data <- data.table::data.table(
    witch12 = rep(regs, 2),
    t = rep(c("1", "2"), each = length(regs)),
    value = c(
      as.numeric(seq_along(regs)),
      2 * as.numeric(seq_along(regs))
    )
  )

  res <- suppressMessages(convert_item(
    .data, "witch17", "t30", "n", data.table::data.table(
      parameter = character(), type = character(), value = character()
    ),
    witchtools::time_mappings, witchtools::region_mappings,
    witchtools::default_weights, "witch17", "t30", "NA"
  ))
  out <- res[[1]]

  expect_equal(colnames(out), c("n", "t", "value"))
  # t == 1 -> 2005 and t == 2 -> 2010, so the second period doubles the first
  expect_equal(
    out[n == "usa" & t == "2"]$value,
    2 * out[n == "usa" & t == "1"]$value
  )
})

test_that("convert_item leaves the region column alone when it already matches reg_id", {
  regs <- unique(region_mappings[["witch17"]][["witch17"]])
  .data <- data.table::data.table(
    witch17 = regs,
    year = 2005,
    value = as.numeric(seq_along(regs))
  )

  res <- suppressMessages(convert_item(
    .data, "witch17", "t30", "n", data.table::data.table(
      parameter = character(), type = character(), value = character()
    ),
    witchtools::time_mappings, witchtools::region_mappings,
    witchtools::default_weights, "witch17", "t30", "NA"
  ))
  out <- res[[1]]

  expect_equal(colnames(out), c("n", "t", "value"))
  expect_equal(
    out[t == "1"][order(n)]$value,
    data.table::data.table(n = regs, value = as.numeric(seq_along(regs)))[
      order(n)
    ]$value
  )
})

test_that("convert_item skips time conversion when time_id is 'year'", {
  regs <- unique(region_mappings[["witch12"]][["witch12"]])
  .data <- data.table::data.table(
    witch12 = regs,
    year = 2005,
    value = as.numeric(seq_along(regs))
  )

  res <- suppressMessages(convert_item(
    .data, "witch17", "year", "n", data.table::data.table(
      parameter = character(), type = character(), value = character()
    ),
    witchtools::time_mappings, witchtools::region_mappings,
    witchtools::default_weights, "witch17", "t30", "NA"
  ))
  out <- res[[1]]

  expect_true("year" %in% colnames(out))
  expect_false("t" %in% colnames(out))
  expect_equal(unique(out$year), 2005)
})

test_that("convert_item uses the 'nagg' operator and returns info for sumby", {
  regs <- unique(region_mappings[["witch12"]][["witch12"]])
  .data <- data.table::data.table(
    witch12 = regs,
    year = 2005,
    value = as.numeric(seq_along(regs))
  )

  item_param <- data.table::data.table(
    parameter = "mypar", type = "nagg", value = "sumby"
  )

  res <- suppressMessages(convert_item(
    .data, "witch17", "t30", "n", item_param,
    witchtools::time_mappings, witchtools::region_mappings,
    witchtools::default_weights, "witch17", "t30", "NA"
  ))

  expect_true(data.table::is.data.table(res[[2]]))
  expect_equal(colnames(res[[2]]), c("n", "t", "value"))
  # coverage shares are within [0, 1]
  expect_true(all(res[[2]]$value >= 0 & res[[2]]$value <= 1 + 1e-9))
})

test_that("convert_item forces the constant weight for min and max operators", {
  regs <- unique(region_mappings[["witch12"]][["witch12"]])
  .data <- data.table::data.table(
    witch12 = regs,
    year = 2005,
    value = as.numeric(seq_along(regs))
  )

  item_param <- data.table::data.table(
    parameter = "mypar", type = "nagg", value = "max"
  )

  # 'cst' is deliberately removed: if the operator did not force it, the
  # function would silently use another weight instead of failing.
  weights <- witchtools::default_weights[
    setdiff(names(witchtools::default_weights), "cst")
  ]

  expect_error(
    suppressMessages(convert_item(
      .data, "witch17", "t30", "n", item_param,
      witchtools::time_mappings, witchtools::region_mappings,
      weights, "witch17", "t30", "NA"
    )),
    "cst not in weights"
  )
})

test_that("convert_item errors when the requested weight is unknown", {
  regs <- unique(region_mappings[["witch12"]][["witch12"]])
  .data <- data.table::data.table(
    witch12 = regs,
    year = 2005,
    value = as.numeric(seq_along(regs))
  )

  item_param <- data.table::data.table(
    parameter = "mypar", type = "nweight", value = "not_a_weight"
  )

  expect_error(
    suppressMessages(convert_item(
      .data, "witch17", "t30", "n", item_param,
      witchtools::time_mappings, witchtools::region_mappings,
      witchtools::default_weights, "witch17", "t30", "NA"
    )),
    "not_a_weight not in weights"
  )
})

test_that("convert_item applies the 'tagg' time aggregator", {
  regs <- unique(region_mappings[["witch12"]][["witch12"]])
  .data <- data.table::data.table(
    witch12 = rep(regs, 3),
    year = rep(c(2005, 2006, 2007), each = length(regs)),
    value = rep(c(1, 2, 3), each = length(regs))
  )

  item_param <- data.table::data.table(
    parameter = "mypar", type = "tagg", value = "sum"
  )

  res_sum <- suppressMessages(convert_item(
    data.table::copy(.data), "witch17", "t30", "n", item_param,
    witchtools::time_mappings, witchtools::region_mappings,
    witchtools::default_weights, "witch17", "t30", "NA"
  ))[[1]]

  res_mean <- suppressMessages(convert_item(
    data.table::copy(.data), "witch17", "t30", "n", data.table::data.table(
      parameter = character(), type = character(), value = character()
    ),
    witchtools::time_mappings, witchtools::region_mappings,
    witchtools::default_weights, "witch17", "t30", "NA"
  ))[[1]]

  # 2005, 2006 and 2007 all fall in the first t30 period
  expect_equal(
    res_sum[n == "usa" & t == "1"]$value,
    (1 + 2 + 3) / mean(c(1, 2, 3)) * res_mean[n == "usa" & t == "1"]$value
  )
})

test_that("convert_item does not mutate an unrelated package dataset", {
  before <- data.table::copy(witchtools::region_mappings[["witch17"]])

  regs <- unique(region_mappings[["witch12"]][["witch12"]])
  .data <- data.table::data.table(
    witch12 = regs,
    year = 2005,
    value = as.numeric(seq_along(regs))
  )

  suppressMessages(convert_item(
    .data, "witch17", "t30", "n", data.table::data.table(
      parameter = character(), type = character(), value = character()
    ),
    witchtools::time_mappings, witchtools::region_mappings,
    witchtools::default_weights, "witch17", "t30", "NA"
  ))

  expect_equal(witchtools::region_mappings[["witch17"]], before)
})
