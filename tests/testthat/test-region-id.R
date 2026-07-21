
test_that("region_id returns the name of the non-iso3 column", {
  expect_identical(region_id(witchtools::region_mappings[["witch17"]]), "witch17")
})

test_that("region_id works whatever the column order", {
  rm <- data.table::data.table(ed58 = c("ita", "fra"), iso3 = c("ITA", "FRA"))
  expect_identical(region_id(rm), "ed58")

  data.table::setcolorder(rm, c("iso3", "ed58"))
  expect_identical(region_id(rm), "ed58")
})

test_that("region_id keeps only the first non-iso3 column", {
  rm <- data.table::data.table(iso3 = "ITA", witch17 = "italy", extra = 1L)
  expect_identical(region_id(rm), "witch17")
})

test_that("region_id works on a plain data.frame and on a named list", {
  expect_identical(region_id(data.frame(iso3 = "ITA", n7 = "eu")), "n7")
  expect_identical(region_id(list(iso3 = "ITA", n7 = "eu")), "n7")
})
