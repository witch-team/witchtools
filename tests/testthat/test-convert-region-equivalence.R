# Engine-equivalence tests: the fast pair-coefficient engine must reproduce
# the legacy iso3-explosion engine for every operator, agg_missing mode and
# route (values within floating-point tolerance; identical column names,
# order and classes).

both_engines <- function(expr_fun) {
  list(
    old = with_engine("legacy", expr_fun()),
    new = with_engine("fast", expr_fun())
  )
}

test_that("fast engine matches legacy across operators and agg_missing", {
  for (op in c("sum", "mean", "set1", "min", "minw", "max", "maxw")) {
    for (am in c("NA", "zero")) {
      dt <- make_input("witch17", seed = 11)
      r <- both_engines(function() {
        convert_region(data.table::copy(dt),
          to_reg = "witch20",
          agg_operator = op, agg_missing = am
        )
      })
      expect_region_equal(r$old, r$new)
    }
  }
})

test_that("fast engine matches legacy for sumby including info", {
  dt <- make_input("witch17", seed = 12)
  r <- both_engines(function() {
    convert_region(data.table::copy(dt),
      to_reg = "witch20",
      agg_operator = "sumby", info = TRUE
    )
  })
  expect_region_equal(r$old, r$new)
  expect_true(data.table::is.data.table(r$new$info))
})

test_that("fast engine matches legacy on other routes and weights", {
  for (route_to in c("witch20", "r5")) {
    for (w in c("gdp", "cst", "pop")) {
      dt <- make_input("witch17", seed = 13)
      r <- both_engines(function() {
        convert_region(data.table::copy(dt),
          to_reg = route_to,
          agg_weight = witchtools::default_weights[[w]]
        )
      })
      expect_region_equal(r$old, r$new)
    }
  }
})

test_that("iso3 input routes through the legacy engine and is unchanged", {
  dt <- make_input("iso3", seed = 14)
  r <- both_engines(function() {
    convert_region(data.table::copy(dt), to_reg = "witch17")
  })
  # Both engines must give identical results (same code path).
  expect_identical(r$old, r$new)
})

test_that("regions missing per id-group are handled identically", {
  dt <- make_input("witch17", seed = 15, missing_region_per_id = TRUE)
  for (op in c("sum", "mean")) {
    r <- both_engines(function() {
      convert_region(data.table::copy(dt), to_reg = "witch20", agg_operator = op)
    })
    expect_region_equal(r$old, r$new)
  }
})

test_that("a region absent from the whole table is handled identically", {
  dt <- make_input("witch17", seed = 16, drop_region = "usa")
  r <- both_engines(function() {
    convert_region(data.table::copy(dt), to_reg = "witch20")
  })
  expect_region_equal(r$old, r$new)
})

test_that("NA values propagate identically (sum, mean zero, set1 NA)", {
  dt <- make_input("witch17", seed = 17, na_frac = 0.15)
  for (case in list(
    c("sum", "NA"), c("mean", "zero"), c("mean", "NA"),
    c("set1", "NA"), c("set1", "zero"), c("min", "NA"), c("max", "NA")
  )) {
    r <- both_engines(function() {
      convert_region(data.table::copy(dt),
        to_reg = "witch20",
        agg_operator = case[1], agg_missing = case[2]
      )
    })
    expect_region_equal(r$old, r$new)
  }
})

test_that("all-NA groups vanish under mean/'NA' in both engines", {
  dt <- make_input("witch17", seed = 18)
  dt[variable == "var1", value := NA_real_]
  r <- both_engines(function() {
    convert_region(data.table::copy(dt),
      to_reg = "witch20",
      agg_operator = "mean", agg_missing = "NA"
    )
  })
  expect_false("var1" %in% r$old$variable)
  expect_region_equal(r$old, r$new)
})

test_that("duplicated rows double-count identically (sum and sumby info)", {
  dt <- make_input("witch17", seed = 19, dup_rows = TRUE)
  r <- both_engines(function() {
    convert_region(data.table::copy(dt),
      to_reg = "witch20",
      agg_operator = "sumby", info = TRUE
    )
  })
  expect_region_equal(r$old, r$new)
})

test_that("iso3 missing from the weight table is handled identically", {
  w <- data.table::copy(witchtools::default_weights[["gdp"]])
  w <- w[!iso3 %in% c("FRA", "CHN", "BRA")]
  dt <- make_input("witch17", seed = 20)
  r <- both_engines(function() {
    convert_region(data.table::copy(dt), to_reg = "witch20", agg_weight = w)
  })
  expect_region_equal(r$old, r$new)
})

test_that("NA regions in the target mapping are handled identically", {
  rmap <- data.table::copy(witchtools::region_mappings[["witch20"]])
  rmap[iso3 %in% c("USA", "CAN"), witch20 := NA_character_]
  regions <- witchtools::region_mappings
  regions[["witch20"]] <- rmap
  dt <- make_input("witch17", seed = 21)
  r <- both_engines(function() {
    convert_region(data.table::copy(dt), to_reg = "witch20", regions = regions)
  })
  expect_region_equal(r$old, r$new)
})

test_that("a non-partition from-mapping is handled identically", {
  # One country in two from-regions: defensive check of the sw_from
  # derivation, which assumes per-region denominators.
  rmap <- data.table::copy(witchtools::region_mappings[["witch17"]])
  rmap <- rbind(rmap, data.table::data.table(witch17 = "usa", iso3 = "MEX"))
  regions <- witchtools::region_mappings
  regions[["witch17"]] <- rmap
  dt <- make_input("witch17", seed = 22)
  r <- both_engines(function() {
    convert_region(data.table::copy(dt),
      from_reg = "witch17", to_reg = "witch20", regions = regions
    )
  })
  expect_region_equal(r$old, r$new)
})

test_that("fast engine preserves legacy row order", {
  dt <- make_input("witch17", seed = 23)
  for (op in c("sum", "mean", "min", "max")) {
    r <- both_engines(function() {
      convert_region(data.table::copy(dt), to_reg = "witch20", agg_operator = op)
    })
    expect_identical(
      as.data.frame(r$old)[, setdiff(names(r$old), "value")],
      as.data.frame(r$new)[, setdiff(names(r$new), "value")]
    )
  }
})

test_that("fast engine does not mutate the input table", {
  dt <- make_input("witch17", seed = 24)
  before <- data.table::copy(dt)
  invisible(with_engine("fast", convert_region(dt, to_reg = "witch20")))
  expect_identical(names(dt), names(before))
  expect_equal(as.data.frame(dt), as.data.frame(before))
})

test_that("unknown operators fail with the same error in both engines", {
  dt <- make_input("witch17", seed = 25)
  for (eng in c("legacy", "fast")) {
    expect_error(
      with_engine(eng, convert_region(data.table::copy(dt),
        to_reg = "witch20", agg_operator = "bogus"
      )),
      "not implemented"
    )
  }
})
