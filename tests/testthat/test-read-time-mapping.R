test_that("read time mapping", {
  res1 <- witch_time_mapping(system.file("extdata", "t30.csv", package = "witchtools"))

  res2 <- time_mappings[["t30"]]
  data.table::setindex(res2, NULL)

  expect_equal(res1, res2)
})
