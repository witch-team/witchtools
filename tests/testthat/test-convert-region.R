
test_that("convert region needs a column name value", {

  gdp_iso3 <- data.table::copy(default_weights[['gdp']])

  expect_error(
    convert_region(gdp_iso3, to_reg = 'witch17')
  )

})

test_that("convert region with a simple ISO3 example", {

  gdp_iso3 <- data.table::copy(default_weights[['gdp']])
  data.table::setnames(gdp_iso3,'weight','value')
  res1 <- convert_region(gdp_iso3, to_reg = 'witch17')

  gdp_iso3 <- data.table::copy(default_weights[['gdp']])
  data.table::setnames(gdp_iso3,'weight','value')
  gdp_iso3 <- merge(gdp_iso3, region_mappings[['witch17']], by = "iso3")
  res2 <- gdp_iso3[, .(value = sum(value)), by = "witch17" ]

  data.table::setkey(res1,witch17)
  data.table::setkey(res2,witch17)

  expect_equal(res1,res2)

})

test_that("convert region with a simple ISO3 example, whatever is the weight", {

  gdp_iso3 <- data.table::copy(default_weights[['gdp']])
  data.table::setnames(gdp_iso3,'weight','value')
  res1 <- convert_region(gdp_iso3, to_reg = 'witch17',
                         agg_weight = default_weights[['pop']])

  gdp_iso3 <- data.table::copy(default_weights[['gdp']])
  data.table::setnames(gdp_iso3,'weight','value')
  gdp_iso3 <- merge(gdp_iso3, region_mappings[['witch17']], by = "iso3")
  res2 <- gdp_iso3[, .(value = sum(value)), by = "witch17" ]

  data.table::setkey(res1,witch17)
  data.table::setkey(res2,witch17)

  expect_equal(res1,res2)

})

test_that("convert region with a simple ISO3 example, returns list with info", {

  gdp_iso3 <- data.table::copy(default_weights[['gdp']])
  data.table::setnames(gdp_iso3,'weight','value')
  res1 <- convert_region(gdp_iso3, to_reg = 'witch17',
                         info = TRUE)

  gdp_iso3 <- data.table::copy(default_weights[['gdp']])
  data.table::setnames(gdp_iso3,'weight','value')
  gdp_iso3 <- merge(gdp_iso3, region_mappings[['witch17']], by = "iso3")
  res2 <- gdp_iso3[, .(value = sum(value)), by = "witch17" ]

  data.table::setkey(res1$data,witch17)
  data.table::setkey(res2,witch17)

  expect_equal(res1$data,res2)

})
