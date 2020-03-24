test_that("convert region needs data.table", {

  # load time mapping t30
  f <- file.path(system.file("timescale",package = "witchtools"),"t30.csv")
  tm <- load_timescale_mapping(f)

  dd <- data.frame(z = c(1,2), year = 2005:2050, value = 1:46)

  expect_error(
    convert_time_period(dd, tm)
  )

})

