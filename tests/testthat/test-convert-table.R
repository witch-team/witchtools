test_that("convert_table converts both years and regions", {
  dt <- data.table::data.table(
    iso3 = rep(c("FRA", "DEU", "ITA"), 2),
    year = rep(c(2005, 2010), each = 3),
    value = as.numeric(1:6)
  )

  res <- convert_table(dt,
    to_reg = "witch17",
    time_mapping = "t30",
    agg_weight = default_weights[["gdp"]],
    regions = region_mappings
  )

  expect_true(data.table::is.data.table(res))
  expect_equal(sort(colnames(res)), sort(c("t", "witch17", "value")))

  data.table::setkey(res, t)

  # FRA + DEU + ITA all belong to 'europe'; 2005 -> t1, 2010 -> t2
  expect_equal(res$witch17, c("europe", "europe"))
  expect_equal(res$t, c("1", "2"))
  expect_equal(res$value, c(1 + 2 + 3, 4 + 5 + 6))
})

test_that("convert_table aggregation matches an independent data.table computation", {
  gdp <- data.table::copy(default_weights[["gdp"]])
  data.table::setnames(gdp, "weight", "value")
  gdp[, year := 2005]

  res1 <- convert_table(data.table::copy(gdp),
    to_reg = "witch17",
    time_mapping = "t30",
    agg_weight = default_weights[["gdp"]],
    regions = region_mappings
  )

  gdp2 <- data.table::copy(default_weights[["gdp"]])
  data.table::setnames(gdp2, "weight", "value")
  gdp2 <- merge(gdp2, region_mappings[["witch17"]], by = "iso3")
  res2 <- gdp2[, .(value = sum(value)), by = "witch17"]
  res2[, t := "1"]
  data.table::setcolorder(res2, c("t", "witch17", "value"))

  data.table::setkey(res1, witch17)
  data.table::setkey(res2, witch17)

  expect_equal(res1, res2)
})

test_that("convert_table with do_region = FALSE only converts time", {
  dt <- data.table::data.table(
    iso3 = rep(c("FRA", "DEU", "ITA"), 2),
    year = rep(c(2005, 2010), each = 3),
    value = as.numeric(1:6)
  )

  res <- convert_table(dt, time_mapping = "t30", do_region = FALSE)

  expect_equal(sort(colnames(res)), sort(c("iso3", "t", "value")))
  expect_equal(nrow(res), 6)
  data.table::setkey(res, t, iso3)
  expect_equal(res[t == "1"][order(iso3)]$value, c(2, 1, 3)) # DEU, FRA, ITA
})

test_that("convert_table with do_time_period = FALSE only converts regions", {
  dt <- data.table::data.table(
    iso3 = c("FRA", "DEU", "ITA", "USA"),
    value = as.numeric(1:4)
  )

  res <- convert_table(dt,
    to_reg = "witch17",
    do_time_period = FALSE,
    agg_weight = default_weights[["gdp"]],
    regions = region_mappings
  )

  expect_equal(sort(colnames(res)), sort(c("witch17", "value")))
  data.table::setkey(res, witch17)
  expect_equal(res$witch17, c("europe", "usa"))
  expect_equal(res$value, c(1 + 2 + 3, 4))
})

test_that("convert_table passes time options through ... and through options", {
  dt <- data.table::data.table(
    iso3 = "FRA",
    year = c(2005, 2006, 2007),
    value = c(1, 2, 3)
  )

  res_dots <- convert_table(data.table::copy(dt),
    to_reg = "witch17",
    time_mapping = "t30",
    time_aggregate = "sum",
    agg_weight = default_weights[["gdp"]],
    regions = region_mappings
  )

  res_opts <- convert_table(data.table::copy(dt),
    to_reg = "witch17",
    time_mapping = "t30",
    options = list(time_aggregate = "sum"),
    agg_weight = default_weights[["gdp"]],
    regions = region_mappings
  )

  # 2005, 2006 and 2007 all belong to the first period of t30
  expect_equal(res_dots$value, 1 + 2 + 3)
  expect_equal(res_dots, res_opts)

  # while the default (mean) gives the average
  res_mean <- convert_table(data.table::copy(dt),
    to_reg = "witch17",
    time_mapping = "t30",
    agg_weight = default_weights[["gdp"]],
    regions = region_mappings
  )
  expect_equal(res_mean$value, mean(c(1, 2, 3)))
})

test_that("convert_table passes agg_operator through to convert_region", {
  dt <- data.table::data.table(
    iso3 = c("FRA", "DEU", "ITA"),
    year = 2005,
    value = c(1, 2, 3)
  )

  res <- convert_table(dt,
    to_reg = "witch17",
    time_mapping = "t30",
    agg_operator = "mean",
    agg_weight = default_weights[["cst"]],
    regions = region_mappings
  )

  # 'europe' has more countries than the three provided, the missing ones
  # are dropped, so a constant-weight mean is the plain mean of 1, 2, 3
  expect_equal(res$value, mean(c(1, 2, 3)))
})

test_that("convert_table with info = FALSE returns a bare data.table", {
  dt <- data.table::data.table(
    iso3 = c("FRA", "DEU", "ITA"),
    year = 2005,
    value = c(1, 2, 3)
  )

  res <- convert_table(dt,
    to_reg = "witch17",
    time_mapping = "t30",
    agg_weight = default_weights[["gdp"]],
    regions = region_mappings
  )

  expect_true(data.table::is.data.table(res))
  expect_false(is.list(res) && identical(names(res), c("data", "info")))
})

test_that("convert_table with info = TRUE returns a list with data and info", {
  dt <- data.table::data.table(
    iso3 = c("FRA", "DEU", "ITA"),
    year = 2005,
    value = c(1, 2, 3)
  )

  res <- convert_table(dt,
    to_reg = "witch17",
    time_mapping = "t30",
    agg_weight = default_weights[["gdp"]],
    regions = region_mappings,
    info = TRUE
  )

  expect_equal(names(res), c("data", "info"))
  expect_true(data.table::is.data.table(res$data))
  expect_equal(res$data$value, 6)
  # the 'sum' operator does not produce coverage information
  expect_null(res$info)
})

test_that("convert_table with sumby reports the coverage in info", {
  dt <- data.table::data.table(
    year = 2005,
    iso3eur = c("eur", "usa"),
    value = c(10000, 5000)
  )

  agg_weight <- data.table::copy(default_weights[["gdp"]])

  res <- convert_table(dt,
    from_reg = "iso3eur",
    to_reg = "witch17",
    time_mapping = "t30",
    agg_operator = "sumby",
    agg_weight = agg_weight,
    regions = region_mappings,
    info = TRUE
  )

  expect_equal(names(res), c("data", "info"))
  expect_true(data.table::is.data.table(res$info))

  europe <- region_mappings[["witch17"]][witch17 == "europe", iso3]
  eu27 <- region_mappings[["iso3eur"]][iso3eur == "eur", iso3]
  share_eur <- sum(agg_weight[iso3 %in% eu27, weight]) /
    sum(agg_weight[iso3 %in% europe, weight])

  data.table::setkey(res$data, witch17)
  data.table::setkey(res$info, witch17)

  expect_equal(res$data$value, c(10000, 5000))
  expect_equal(res$info$value, c(share_eur, 1))
})

test_that("convert_table errors when regions is not given", {
  dt <- data.table::data.table(
    iso3 = c("FRA", "DEU"),
    year = 2005,
    value = c(1, 2)
  )

  # convert_table declares regions = NULL and overrides convert_region default
  expect_error(
    convert_table(dt,
      to_reg = "witch17",
      time_mapping = "t30",
      agg_weight = default_weights[["gdp"]]
    ),
    "witch17"
  )
})

test_that("convert_table errors when agg_weight is not given", {
  dt <- data.table::data.table(
    iso3 = c("FRA", "DEU"),
    year = 2005,
    value = c(1, 2)
  )

  # convert_table declares agg_weight = NULL and overrides convert_region default
  expect_error(
    convert_table(dt,
      to_reg = "witch17",
      time_mapping = "t30",
      regions = region_mappings
    ),
    "agg_weight is NULL"
  )
})

test_that("convert_table errors when the table has no value column", {
  dt <- data.table::data.table(
    iso3 = c("FRA", "DEU"),
    year = 2005,
    val = c(1, 2)
  )

  expect_error(
    convert_table(dt,
      to_reg = "witch17",
      time_mapping = "t30",
      agg_weight = default_weights[["gdp"]],
      regions = region_mappings
    )
  )
})
