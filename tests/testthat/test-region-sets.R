
test_that("witch17 eu_region is europe", {
  expect_equal("europe", eu_regions(region_mappings[["witch17"]]))
})

test_that("witch34 eu_region contains ", {
  regs <- c("aut","balkan","bnl","cze","deu","easteu","esp","fra",
            "gbr","grc","irl","ita","northeu","pol","prt","rou",
            "swe")
  regs <- sort(tolower(regs))
  expect_equal(regs, eu_regions(region_mappings[["witch34"]]))
})
