test_that("read time mapping", {
  res1 <- witch_time_mapping("t30.csv")

  res2 <- time_mappings[["t30"]]

  expect_equal(res1, res2)
})
