test_that("convert time simple use case", {
  dd <- data.table::data.table(z = c(1, 2), year = 2005:2050, value = 1:46)

  res1 <- convert_time_period(dd, time_mappings[["t30"]])

  res2 <- data.table::data.table(
    z = c(1, 2),
    t = as.character(rep(1:10, each = 2)),
    value = rep(c(
      mean(1:3),
      vapply(split(
        4:43,
        ceiling(seq_along(4:43) / 5)
      ), mean, c(0)),
      mean(44:46)
    ),
    each = 2
    )
  )

  data.table::setkey(res1, z, t)
  data.table::setkey(res2, z, t)

  expect_equal(res1, res2)
})

test_that("convert time with sum", {
  dd <- data.table::data.table(
    z = c(1, 2),
    year = rep(2005:2050, each = 2),
    value = rep(1:46, each = 2)
  )

  res1 <- convert_time_period(dd,
                              time_mappings[["t30"]],
                              time_aggregate = "sum")

  res2 <- data.table::data.table(
    z = c(1, 2),
    t = as.character(rep(1:10, each = 2)),
    value = rep(c(
      sum(1:3),
      vapply(split(
        4:43,
        ceiling(seq_along(4:43) / 5)
      ), sum, c(0)),
      sum(44:46)
    ),
    each = 2
    )
  )

  data.table::setkey(res1, z, t)
  data.table::setkey(res2, z, t)

  expect_equal(res1, res2)
})

test_that("convert time no interpolation and no extrapolation", {
  tm <- time_mappings[["t30"]]
  tm <- tm[year %in% 2003:2027, .(year, t, refyear)]

  dd <- data.table::data.table(year = c(2010, 2020), value = 1:2)

  res1 <- convert_time_period(dd, tm)

  data.table::setkey(res1, t)

  res2 <- data.table::data.table(t = as.character(c(2, 4)), value = c(1, 2))
  data.table::setkey(res2, t)

  expect_equal(res1, res2)
})

test_that("convert time no interpolation and extrapolation", {
  tm <- time_mappings[["t30"]]
  tm <- tm[year %in% 2003:2027, .(year, t, refyear)]

  dd <- data.table::data.table(year = c(2010, 2020), value = 1:2)

  res1 <- convert_time_period(dd, tm, do_extrap = T, verbose = F)

  data.table::setkey(res1, t)

  res2 <- data.table::data.table(
    t = as.character(c(1, 2, 4, 5)),
    value = c(1, 1, 2, 2)
  )
  data.table::setkey(res2, t)

  expect_equal(res1, res2)
})

test_that("convert time interpolation and no extrapolation", {
  tm <- time_mappings[["t30"]]
  tm <- tm[year %in% 2003:2027, .(year, t, refyear)]

  dd <- data.table::data.table(year = c(2010, 2020), value = 1:2)

  res1 <- convert_time_period(dd, tm, do_interp = T, verbose = F)

  data.table::setkey(res1, t)

  res2 <- data.table::data.table(t = as.character(2:4), value = c(1, 1.5, 2))
  data.table::setkey(res2, t)

  expect_equal(res1, res2)
})

test_that("convert time interpolation and no extrap, but with past extrap", {
  tm <- time_mappings[["t30"]]
  tm <- tm[year %in% 2003:2027, .(year, t, refyear)]

  dd <- data.table::data.table(year = c(2010, 2020), value = 1:2)

  res1 <- convert_time_period(dd, tm,
    do_interp = T, do_past_extrap = T,
    verbose = F
  )

  data.table::setkey(res1, t)

  res2 <- data.table::data.table(t = as.character(1:4), value = c(1, 1, 1.5, 2))
  data.table::setkey(res2, t)

  expect_equal(res1, res2)
})

test_that("convert time interpolation and extrapolation", {
  tm <- time_mappings[["t30"]]
  tm <- tm[year %in% 2003:2027, .(year, t, refyear)]

  dd <- data.table::data.table(year = c(2010, 2020), value = 1:2)

  res1 <- convert_time_period(dd, tm,
    do_interp = TRUE, do_extrap = TRUE,
    verbose = FALSE
  )

  data.table::setkey(res1, t)

  res2 <- data.table::data.table(t = as.character(1:5), value = c(1, 1, 1.5, 2, 2))
  data.table::setkey(res2, t)

  expect_equal(res1, res2)
})

test_that("convert time with exponential interpolation", {
  tm <- time_mappings[["t30"]]
  tm <- tm[year %in% 2003:2027, .(year, t, refyear)]

  # Data with exponential growth: 100 -> 200 (doubles in 10 years)
  dd <- data.table::data.table(year = c(2010, 2020), value = c(100, 200))

  res1 <- convert_time_period(dd, tm,
    do_interp = TRUE,
    interp_method = "exponential",
    verbose = FALSE
  )

  data.table::setkey(res1, t)

  # Exponential interpolation: geometric mean at midpoint
  # sqrt(100 * 200) = 141.42...
  res2 <- data.table::data.table(
    t = as.character(2:4),
    value = c(100, sqrt(100 * 200), 200)
  )
  data.table::setkey(res2, t)

  expect_equal(res1, res2, tolerance = 1e-6)
})

test_that("convert time with spline interpolation", {
  tm <- time_mappings[["t30"]]
  tm <- tm[year %in% 2003:2027, .(year, t, refyear)]

  dd <- data.table::data.table(year = c(2010, 2020), value = c(1, 2))

  res1 <- convert_time_period(dd, tm,
    do_interp = TRUE,
    interp_method = "spline",
    verbose = FALSE
  )

  data.table::setkey(res1, t)

  # Spline with only 2 points should be equivalent to linear
  res2 <- data.table::data.table(
    t = as.character(2:4),
    value = c(1, 1.5, 2)
  )
  data.table::setkey(res2, t)

  expect_equal(res1, res2, tolerance = 1e-6)
})

test_that("exponential interpolation with negative values falls back to linear", {
  tm <- time_mappings[["t30"]]
  tm <- tm[year %in% 2003:2027, .(year, t, refyear)]

  dd <- data.table::data.table(year = c(2010, 2020), value = c(-1, 2))

  # Should produce a warning and fall back to linear
  expect_warning(
    res1 <- convert_time_period(dd, tm,
      do_interp = TRUE,
      interp_method = "exponential",
      verbose = FALSE
    ),
    "Exponential interpolation requires positive values"
  )

  data.table::setkey(res1, t)

  # Should use linear interpolation as fallback
  res2 <- data.table::data.table(
    t = as.character(2:4),
    value = c(-1, 0.5, 2)
  )
  data.table::setkey(res2, t)

  expect_equal(res1, res2)
})

test_that("interpolation methods preserve constant extrapolation", {
  tm <- time_mappings[["t30"]]
  tm <- tm[year %in% 2003:2027, .(year, t, refyear)]

  dd <- data.table::data.table(year = c(2010, 2020), value = c(100, 200))

  # Test with exponential interpolation and extrapolation
  res1 <- convert_time_period(dd, tm,
    do_interp = TRUE,
    do_extrap = TRUE,
    interp_method = "exponential",
    verbose = FALSE
  )

  data.table::setkey(res1, t)

  # Extrapolation should be constant (100 before, 200 after)
  # even though interpolation is exponential
  res2 <- data.table::data.table(
    t = as.character(1:5),
    value = c(100, 100, sqrt(100 * 200), 200, 200)
  )
  data.table::setkey(res2, t)

  expect_equal(res1, res2, tolerance = 1e-6)
})

test_that("invalid interpolation method raises error", {
  dd <- data.table::data.table(year = 2005:2050, value = 1:46)

  expect_error(
    convert_time_period(dd, "t30", interp_method = "invalid"),
    "interp_method must be 'linear', 'exponential', or 'spline'"
  )
})

test_that("spline interpolation with natural method", {
  tm <- time_mappings[["t30"]]
  tm <- tm[year %in% 2003:2027, .(year, t, refyear)]

  dd <- data.table::data.table(year = c(2010, 2020), value = c(100, 200))

  res1 <- convert_time_period(dd, tm,
    do_interp = TRUE,
    interp_method = "spline_natural",
    verbose = FALSE
  )

  data.table::setkey(res1, t)

  # Should complete without error and have 3 time periods
  expect_equal(nrow(res1), 3)
  expect_equal(res1$t, as.character(2:4))
  expect_equal(res1$value[1], 100)
  expect_equal(res1$value[3], 200)
})

test_that("spline interpolation with monoH.FC method preserves monotonicity", {
  tm <- time_mappings[["t30"]]
  tm <- tm[year %in% 2003:2027, .(year, t, refyear)]

  # Monotonically increasing data
  dd <- data.table::data.table(year = c(2010, 2020), value = c(100, 200))

  res1 <- convert_time_period(dd, tm,
    do_interp = TRUE,
    interp_method = "spline_monoH.FC",
    verbose = FALSE
  )

  data.table::setkey(res1, t)

  # Check monotonicity: values should be increasing
  expect_true(all(diff(res1$value) >= 0))
  expect_equal(res1$value[1], 100)
  expect_equal(res1$value[3], 200)
})

test_that("spline interpolation with hyman method", {
  tm <- time_mappings[["t30"]]
  tm <- tm[year %in% 2003:2027, .(year, t, refyear)]

  dd <- data.table::data.table(year = c(2010, 2020), value = c(100, 200))

  res1 <- convert_time_period(dd, tm,
    do_interp = TRUE,
    interp_method = "spline_hyman",
    verbose = FALSE
  )

  data.table::setkey(res1, t)

  # Hyman method should preserve monotonicity
  expect_true(all(diff(res1$value) >= 0))
  expect_equal(res1$value[1], 100)
  expect_equal(res1$value[3], 200)
})

test_that("invalid spline method raises error", {
  dd <- data.table::data.table(year = 2005:2050, value = 1:46)

  expect_error(
    convert_time_period(dd, "t30", interp_method = "spline_invalid"),
    "Invalid spline method"
  )
})

test_that("invalid spline format raises error", {
  dd <- data.table::data.table(year = 2005:2050, value = 1:46)

  expect_error(
    convert_time_period(dd, "t30", interp_method = "spline_method_extra"),
    "Invalid interp_method format"
  )
})
