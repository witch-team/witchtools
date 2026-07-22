# Helpers for the convert_region engine-equivalence tests.

# Run expr with a given region-conversion engine ("legacy" or "fast").
with_engine <- function(engine, expr) {
  withr::with_options(
    list(witchtools.convert_region_engine = engine),
    expr
  )
}

# Build a GLOBIOM-shaped input table.
#
# Columns mimic the real GLOBIOM reporting tables: character id columns
# (variable, scenario), one factor id column (ghg_class), a character period
# column (t), a region column named after `route`, and a numeric value.
#
# route: "witch17" (region-level input) or "iso3" (country-level input).
# n_var: number of distinct `variable` values (drives table size).
# na_frac: fraction of values set to NA.
# missing_region_per_id: randomly drop some regions within some id-groups
#   (exercises the data-dependence edge case).
# drop_region: name of one region to remove from the whole table ("" = none).
# dup_rows: duplicate a handful of (id, region) rows.
make_input <- function(route = "witch17",
                       n_var = 4,
                       seed = 42,
                       na_frac = 0,
                       missing_region_per_id = FALSE,
                       drop_region = "",
                       dup_rows = FALSE) {
  set.seed(seed)
  regs <- if (route == "iso3") {
    witchtools::region_mappings[["witch17"]]$iso3
  } else {
    unique(witchtools::region_mappings[[route]][[route]])
  }
  dt <- data.table::CJ(
    variable = paste0("var", seq_len(n_var)),
    scenario = c("ssp1_lu1", "ssp2_lu2"),
    ghg_class = factor(c("GHG001", "GHG002")),
    t = as.character(c(2L, 5L, 10L)),
    region = regs,
    sorted = FALSE
  )
  dt[, value := round(stats::runif(.N, 0, 1000), 3)]
  if (na_frac > 0) {
    idx <- sample(nrow(dt), ceiling(na_frac * nrow(dt)))
    dt[idx, value := NA_real_]
  }
  if (missing_region_per_id) {
    # For a third of the variables, drop a random subset of regions entirely.
    for (v in unique(dt$variable)[seq_len(max(1L, n_var %/% 3L))]) {
      gone <- sample(regs, max(1L, length(regs) %/% 4L))
      dt <- dt[!(variable == v & region %in% gone)]
    }
  }
  if (nzchar(drop_region)) {
    dt <- dt[region != drop_region]
  }
  if (dup_rows) {
    dt <- rbind(dt, dt[sample(nrow(dt), 5L)])
  }
  data.table::setnames(dt, "region", route)
  return(dt)
}

# Compare two convert_region() results (plain data.table, or list(data, info)
# when info = TRUE). Column names, order and classes must be identical; values
# are compared after sorting by all id columns, with a tolerance for the
# floating-point reassociation between engines.
expect_region_equal <- function(old, new, tol = 1e-12) {
  if (is.list(old) && !data.table::is.data.table(old)) {
    testthat::expect_true(is.list(new) && !data.table::is.data.table(new))
    expect_region_equal(old$data, new$data, tol = tol)
    if (is.null(old$info)) {
      testthat::expect_null(new$info)
    } else {
      expect_region_equal(old$info, new$info, tol = tol)
    }
    return(invisible(NULL))
  }
  testthat::expect_identical(names(old), names(new))
  testthat::expect_identical(
    lapply(old, class),
    lapply(new, class)
  )
  keys <- setdiff(names(old), "value")
  o <- data.table::setorderv(data.table::copy(old), keys)
  n <- data.table::setorderv(data.table::copy(new), keys)
  testthat::expect_equal(as.data.frame(o), as.data.frame(n),
    tolerance = tol,
    ignore_attr = TRUE
  )
}
