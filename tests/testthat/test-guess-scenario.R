
test_that("guess_scenario works with linux path", {

  gdxlist <- c('/home/bidule/test1/results_ssp2_bau.gdx',
               '/home/bidule/test2/results_ssp2_bau.gdx')

  scen <- c('test1_bau',
            'test2_bau')

  expect_equal(
    guess_scenario(gdxlist),
    scen
  )

})

test_that("guess_scenario works with windows path and space", {

  gdxlist <- c('C:\\Users\\bidule\\Drobpox (LONG)\\test1\\results_ssp2_bau.gdx',
               'C:\\Users\\bidule\\Drobpox (LONG)\\test2\\results_ssp2_bau.gdx')

  scen <- c('test1_bau',
            'test2_bau')

  skip_on_os("linux")
  expect_equal(
    guess_scenario(gdxlist),
    scen
  )

})

test_that("guess_scenario warns when duplicated scenario name", {

  gdxlist <- c('/home/bidule1/test/results_ssp2_bau.gdx',
               '/home/bidule2/test/results_ssp2_bau.gdx')

  expect_warning(guess_scenario(gdxlist))

})
