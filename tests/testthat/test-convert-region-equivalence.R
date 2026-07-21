# Engine-equivalence tests: the fast pair-coefficient engine must reproduce
# the legacy iso3-explosion engine for every operator, agg_missing mode and
# route. Until the fast engine lands, this file smoke-tests the helpers with
# legacy-vs-legacy comparisons.

test_that("equivalence helpers work (legacy vs legacy smoke)", {
  dt <- make_input("witch17", seed = 1)
  r1 <- with_engine("legacy", convert_region(dt, to_reg = "witch20"))
  r2 <- with_engine("legacy", convert_region(dt, to_reg = "witch20"))
  expect_region_equal(r1, r2)

  ri <- with_engine(
    "legacy",
    suppressWarnings(
      convert_region(dt, to_reg = "witch20", agg_operator = "sumby", info = TRUE)
    )
  )
  expect_region_equal(ri, ri)
  expect_true(data.table::is.data.table(ri$info))
})

test_that("make_input produces the expected shapes", {
  dt <- make_input("witch17", na_frac = 0.1, missing_region_per_id = TRUE,
                   dup_rows = TRUE, seed = 7)
  expect_true("witch17" %in% names(dt))
  expect_true(is.factor(dt$ghg_class))
  expect_type(dt$t, "character")
  expect_gt(sum(is.na(dt$value)), 0)

  di <- make_input("iso3", seed = 7)
  expect_true("iso3" %in% names(di))
})
