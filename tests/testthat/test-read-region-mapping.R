test_that("read region mapping", {
  res1 <- witch_region_mapping("witch17.inc")

  res2 <- region_mappings[["witch17"]]
  data.table::setindex(res2, NULL)

  expect_equal(res1, res2)
})
