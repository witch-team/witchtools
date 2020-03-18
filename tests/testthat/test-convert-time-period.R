test_that("convert time simple use case", {

  # load time mapping t30
  f <- file.path(system.file("timescale",package = "witchtools"),"t30.csv")
  tm <- load_timescale_mapping(f)

  dd <- data.table::data.table(z = c(1,2), year = 2005:2050, value = 1:46)

  res1 <- convert_time_period(dd, tm)

  res2 <- data.table::data.table(z = c(1,2),
                                 t = as.character(rep(1:10, each = 2)),
                                 value = rep(c(mean(1:3),
                                               sapply(split(4:43, ceiling(seq_along(4:43)/5)),mean),
                                               mean(44:46)),
                                             each = 2))

  data.table::setkey(res1,z,t)
  data.table::setkey(res2,z,t)

  expect_equal(res1,res2)

})

test_that("convert time needs data.table", {

  # load time mapping t30
  f <- file.path(system.file("timescale",package = "witchtools"),"t30.csv")
  tm <- load_timescale_mapping(f)

  dd <- data.frame(z = c(1,2), year = 2005:2050, value = 1:46)

  expect_error(
    convert_time_period(dd, tm)
  )

})

test_that("convert time no interpolation and no extrapolation", {

  # load time mapping t30
  f <- file.path(system.file("timescale",package = "witchtools"),"t30.csv")
  tm <- load_timescale_mapping(f)

  tm <- tm[year %in% 2003:2027, .(year,t,refyear)]

  dd <- data.table::data.table(year = c(2010,2020), value = 1:2)

  res1 <- convert_time_period(dd, tm)

  data.table::setkey(res1,t)

  res2 <- data.table::data.table(t = as.character(c(2,4)), value = c(1,2))
  data.table::setkey(res2,t)

  expect_equal(res1,res2)

})

test_that("convert time no interpolation and extrapolation", {

  # load time mapping t30
  f <- file.path(system.file("timescale",package = "witchtools"),"t30.csv")
  tm <- load_timescale_mapping(f)

  tm <- tm[year %in% 2003:2027, .(year,t,refyear)]

  dd <- data.table::data.table(year = c(2010,2020), value = 1:2)

  res1 <- convert_time_period(dd, tm, do_extrap = T, verbose = F)

  data.table::setkey(res1,t)

  res2 <- data.table::data.table(t = as.character(c(1,2,4,5)), value = c(1,1,2,2))
  data.table::setkey(res2,t)

  expect_equal(res1,res2)

})

test_that("convert time interpolation and no extrapolation", {

  # load time mapping t30
  f <- file.path(system.file("timescale",package = "witchtools"),"t30.csv")
  tm <- load_timescale_mapping(f)

  tm <- tm[year %in% 2003:2027, .(year,t,refyear)]

  dd <- data.table::data.table(year = c(2010,2020), value = 1:2)

  res1 <- convert_time_period(dd, tm, do_interp = T, verbose = F)

  data.table::setkey(res1,t)

  res2 <- data.table::data.table(t = as.character(2:4), value = c(1,1.5,2))
  data.table::setkey(res2,t)

  expect_equal(res1,res2)

})

test_that("convert time interpolation and no extrapolation, but with past extrapolation", {

  # load time mapping t30
  f <- file.path(system.file("timescale",package = "witchtools"),"t30.csv")
  tm <- load_timescale_mapping(f)

  tm <- tm[year %in% 2003:2027, .(year,t,refyear)]

  dd <- data.table::data.table(year = c(2010,2020), value = 1:2)

  res1 <- convert_time_period(dd, tm, do_interp = T, do_past_extrap = T, verbose = F)

  data.table::setkey(res1,t)

  res2 <- data.table::data.table(t = as.character(1:4), value = c(1,1,1.5,2))
  data.table::setkey(res2,t)

  expect_equal(res1,res2)

})

test_that("convert time interpolation and extrapolation", {

  # load time mapping t30
  f <- file.path(system.file("timescale",package = "witchtools"),"t30.csv")
  tm <- load_timescale_mapping(f)

  tm <- tm[year %in% 2003:2027, .(year,t,refyear)]

  dd <- data.table::data.table(year = c(2010,2020), value = 1:2)

  res1 <- convert_time_period(dd, tm, do_interp = TRUE, do_extrap = TRUE, verbose = FALSE)

  data.table::setkey(res1,t)

  res2 <- data.table::data.table(t = as.character(1:5), value = c(1,1,1.5,2,2))
  data.table::setkey(res2,t)

  expect_equal(res1,res2)

})
