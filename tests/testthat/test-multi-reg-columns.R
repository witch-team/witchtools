test_that("convert a table with 2 regional columns of the same name", {

  set.seed(42)
  gdp_iso3 <- data.table::copy(default_weights[["gdp"]])
  gdp_iso3[, iso3__2 := sample(iso3)]
  data.table::setnames(gdp_iso3, "weight", "value")
  res1 <- convert_region(gdp_iso3, to_reg = "witch17")

  gdp_iso3 <- data.table::copy(default_weights[["gdp"]])
  gdp_iso3[, iso3__2 := sample(iso3)]
  data.table::setnames(gdp_iso3, "weight", "value")
  rmp <- data.table::copy(region_mappings[["witch17"]])
  gdp_iso3 <- merge(gdp_iso3, rmp, by = "iso3")
  gdp_iso3 <- gdp_iso3[, .(iso3__2,value = sum(value)), by = "witch17"]
  rmp <- data.table::copy(region_mappings[["witch17"]])
  data.table::setnames(rmp, "witch17", "witch17__2")
  data.table::setnames(rmp, "iso3", "iso3__2")
  gdp_iso3 <- merge(gdp_iso3, rmp, by = "iso3__2")
  res2 <- gdp_iso3[, .(value = sum(value)), by = c("witch17","witch17__2")]

  data.table::setkey(res1, witch17, witch17__2)
  data.table::setkey(res2, witch17, witch17__2)

  expect_equal(res1, res2)
})
