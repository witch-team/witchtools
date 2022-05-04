
test_that("convert region needs a column name value", {
  gdp_iso3 <- data.table::copy(default_weights[["gdp"]])

  expect_error(
    convert_region(gdp_iso3, to_reg = "witch17")
  )
})

test_that("convert region with a simple ISO3 example", {
  gdp_iso3 <- data.table::copy(default_weights[["gdp"]])
  data.table::setnames(gdp_iso3, "weight", "value")
  res1 <- convert_region(gdp_iso3, to_reg = "witch17")

  gdp_iso3 <- data.table::copy(default_weights[["gdp"]])
  data.table::setnames(gdp_iso3, "weight", "value")
  gdp_iso3 <- merge(gdp_iso3, region_mappings[["witch17"]], by = "iso3")
  res2 <- gdp_iso3[, .(value = sum(value)), by = "witch17"]

  data.table::setkey(res1, witch17)
  data.table::setkey(res2, witch17)

  expect_equal(res1, res2)
})

test_that("convert region with a simple ISO3 example, whatever is the weight", {
  gdp_iso3 <- data.table::copy(default_weights[["gdp"]])
  data.table::setnames(gdp_iso3, "weight", "value")
  res1 <- convert_region(gdp_iso3,
    to_reg = "witch17",
    agg_weight = default_weights[["pop"]]
  )

  gdp_iso3 <- data.table::copy(default_weights[["gdp"]])
  data.table::setnames(gdp_iso3, "weight", "value")
  gdp_iso3 <- merge(gdp_iso3, region_mappings[["witch17"]], by = "iso3")
  res2 <- gdp_iso3[, .(value = sum(value)), by = "witch17"]

  data.table::setkey(res1, witch17)
  data.table::setkey(res2, witch17)

  expect_equal(res1, res2)
})

test_that("convert region with a simple ISO3 example, returns list with info", {
  gdp_iso3 <- data.table::copy(default_weights[["gdp"]])
  data.table::setnames(gdp_iso3, "weight", "value")
  res1 <- convert_region(gdp_iso3,
    to_reg = "witch17",
    info = TRUE
  )

  gdp_iso3 <- data.table::copy(default_weights[["gdp"]])
  data.table::setnames(gdp_iso3, "weight", "value")
  gdp_iso3 <- merge(gdp_iso3, region_mappings[["witch17"]], by = "iso3")
  res2 <- gdp_iso3[, .(value = sum(value)), by = "witch17"]

  data.table::setkey(res1$data, witch17)
  data.table::setkey(res2, witch17)

  expect_equal(res1$data, res2)
})

test_that("convert with sumby can also downscale reports the right info", {

  .data <- data.table::data.table(year = c(2030,2040), iso3eur = "eur", value = 10000)
  .data <- data.table::rbindlist(list(.data,
                                      data.table::data.table(year = c(2030,2040), iso3eur = "chl", value = 2000)))
  .data <- data.table::rbindlist(list(.data,
                                      data.table::data.table(year = c(2030,2040), iso3eur = "usa", value = 5000)))
  agg_weight <-  witchtools::default_weights[["gdp"]]

  res1 <- convert_region(.data,
                         from_reg = "iso3eur",
                         to_reg = "witch17",
                         agg_operator = "sumby",
                         agg_weight = agg_weight,
                         info = TRUE
  )

  europe <- region_mappings[["witch17"]][witch17=="europe", iso3]
  eu27 <- region_mappings[["iso3eur"]][iso3eur=="eur", iso3]
  weur    <- sum(agg_weight[iso3 %in% eu27, weight])
  weurope <- sum(agg_weight[iso3 %in% europe, weight])
  laca <- region_mappings[["witch17"]][witch17=="laca", iso3]
  wchl <- sum(agg_weight[iso3 %in% "CHL", weight])
  wlaca <- sum(agg_weight[iso3 %in% laca, weight])
  res2 <- c(rep(weur / weurope, 2), rep(wchl / wlaca, 2), rep(1, 2))

  res3 <- c(rep(10000, 2), rep(2000, 2), rep(5000, 2))

  expect_equal(res1$info$value, res2)
  expect_equal(res1$data$value, res3)

})
