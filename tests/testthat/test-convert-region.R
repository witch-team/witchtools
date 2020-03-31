test_that("convert region needs data.table", {

  dd <- data.frame(z = c(1,2), year = 2005:2050, value = 1:46)

  expect_error(
    convert_time_period(dd, time_mappings[['t30']])
  )

})

