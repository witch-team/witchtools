test_that("read region mappins", {

  res1 <- witch_region_mapping('witch17.inc')

  res2 <- region_mappings[['witch17']]

  expect_equal(res1,res2)

})
