
test_that("witch_scen_name strips path, ssp prefix and gdx extension", {
  expect_identical(witch_scen_name("results_ssp2_ctax.gdx"), "ctax")
  expect_identical(witch_scen_name("/some/path/results_ssp5_bau.gdx"), "bau")
})

test_that("witch_scen_name keeps the name when there is no ssp prefix", {
  expect_identical(witch_scen_name("results_bau.gdx"), "results_bau")
  expect_identical(witch_scen_name("bau.gdx"), "bau")
})

test_that("witch_scen_name only removes the literal .gdx extension", {
  expect_identical(witch_scen_name("myXgdx_ssp2_bau.gdx"), "myXgdx_ssp2_bau")
})

test_that("witch_scen_name is vectorised over several files", {
  expect_identical(
    witch_scen_name(c("a/results_ssp1_bau.gdx", "b/results_ssp2_ctax.gdx")),
    c("bau", "ctax")
  )
})

test_that("witch_scen_name returns character(0) on empty input", {
  expect_identical(witch_scen_name(character(0)), character(0))
})
