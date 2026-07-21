
test_that("witch_results_files lists gdx files matching the default pattern", {
  dir <- withr::local_tempdir()
  file.create(file.path(dir, c("results_bau.gdx", "results_ctax.gdx",
                               "other_run.gdx", "results_bau.lst")))

  expect_identical(sort(witch_results_files(dir)),
                   c("results_bau.gdx", "results_ctax.gdx"))
})

test_that("witch_results_files returns nothing when no file matches", {
  dir <- withr::local_tempdir()
  file.create(file.path(dir, "other_run.gdx"))

  expect_length(witch_results_files(dir), 0)
  expect_length(witch_results_files(withr::local_tempdir()), 0)
})

test_that("witch_results_files accepts several restrict patterns and dedups", {
  dir <- withr::local_tempdir()
  file.create(file.path(dir, c("results_bau.gdx", "results_ctax.gdx",
                               "scenario_x.gdx")))

  res <- witch_results_files(dir, restrict = c("^results_", "ctax", "^scenario"))

  expect_identical(sort(res),
                   c("results_bau.gdx", "results_ctax.gdx", "scenario_x.gdx"))
  expect_identical(anyDuplicated(res), 0L)
})

test_that("witch_results_files drops empty restrict patterns", {
  dir <- withr::local_tempdir()
  file.create(file.path(dir, c("results_bau.gdx", "other_run.gdx")))

  expect_identical(witch_results_files(dir, restrict = c("", "^results_")),
                   "results_bau.gdx")
  expect_null(witch_results_files(dir, restrict = ""))
})

test_that("witch_results_files recurses only when asked", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "sub"))
  file.create(file.path(dir, "results_top.gdx"))
  file.create(file.path(dir, "sub", "results_deep.gdx"))

  expect_identical(witch_results_files(dir), "results_top.gdx")
  expect_identical(sort(witch_results_files(dir, recursive = TRUE)),
                   c("results_top.gdx", "sub/results_deep.gdx"))
})

test_that("witch_results_files normalizes names against the search path", {
  dir <- withr::local_tempdir()
  file.create(file.path(dir, "results_bau.gdx"))

  res <- witch_results_files(dir, normalize = TRUE)

  expect_identical(res, normalizePath(file.path(dir, "results_bau.gdx")))
})
