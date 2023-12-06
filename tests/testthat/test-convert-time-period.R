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
