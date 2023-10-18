
suppressPackageStartupMessages(library(gdxtools))
invisible(capture.output({gdxtools::igdx(dirname(Sys.which("gams")))}))

test_that("witch_query returns a table combining scenarios", {

  gdx1 <- withr::local_tempfile(pattern = "results_", fileext = ".gdx")
  gdx2 <- withr::local_tempfile(pattern = "results_", fileext = ".gdx")

  param1 = data.frame(x=c('a','b','d','c'),value=1:4)
  param2 = data.frame(x=c('e','f','a','c'),value=11:14)

  gdxtools::write.gdx(gdx1, params = list(mypar = param1))
  gdxtools::write.gdx(gdx2, params = list(mypar = param2))

  res <- witch_query("mypar", c(gdx1, gdx2))

  expect_equal(sort(paste(res$x,res$value)),
          sort(c(paste(param1$x,param1$value),paste(param2$x,param2$value))))

  expect_true(length(unique(res$scenario)) == 2)

})

test_that("witch_query can aggregate value with filter n='world'", {

  gdx1 <- withr::local_tempfile(pattern = "results_", fileext = ".gdx")
  gdx2 <- withr::local_tempfile(pattern = "results_", fileext = ".gdx")

  param1 = data.frame(n=c('a','b','d','c'),value=1:4)
  param2 = data.frame(n=c('e','f','a','c'),value=-(1:4))

  gdxtools::write.gdx(gdx1, params = list(mypar = param1))
  gdxtools::write.gdx(gdx2, params = list(mypar = param2))

  res <- witch_query("mypar", c(gdx1, gdx2), filter = c(n = "world"))

  expect_equal(sort(res$value), c(-sum(1:4), sum(1:4)))

  expect_true(length(unique(res$scenario)) == 2)
  expect_true(unique(res$n) == "world")

  #TODO TEST colorder

})

test_that("witch_query can aggregate value with filter c(n='world,usa')", {

  gdx1 <- withr::local_tempfile(pattern = "results_", fileext = ".gdx")
  gdx2 <- withr::local_tempfile(pattern = "results_", fileext = ".gdx")

  param1 = data.frame(n=c('usa','a','b','d','c'),value=1:5)
  param2 = data.frame(n=c('e','f','usa','c'),value=-(1:4))

  gdxtools::write.gdx(gdx1, params = list(mypar = param1))
  gdxtools::write.gdx(gdx2, params = list(mypar = param2))

  res <- witch_query("mypar", c(gdx1, gdx2), filter = c(n = "world,usa"))

  expect_equal(sort(res[n=="world"]$value), sort(c(sum(1:5), -sum(1:4))))
  expect_equal(sort(res[n=="usa"]$value), sort(c(1, -3)))

  expect_true(length(unique(res$scenario)) == 2)
  expect_equal(sort(unique(res$n)), sort(c("world","usa")))

  #TODO TEST colorder

})

#TODO test year and time_period

