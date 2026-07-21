test_that("read region mapping", {
  res1 <- witch_region_mapping(system.file("extdata", "witch17.inc", package = "witchtools"))

  res2 <- region_mappings[["witch17"]]
  data.table::setindex(res2, NULL)

  expect_equal(res1, res2)
})

test_that("read region mapping with empty lines", {
  res1 <- witch_region_mapping(system.file("extdata", "ed58.inc", package = "witchtools"))

  res2 <- region_mappings[["ed58"]]
  data.table::setindex(res2, NULL)

  expect_equal(res1, res2)
})
