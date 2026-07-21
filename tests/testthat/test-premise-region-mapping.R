
test_that("premise_region_mapping writes a parsable json with all regions", {
  path <- withr::local_tempfile(fileext = ".json")

  witchtools::premise_region_mapping(filename = path)
  j <- jsonlite::fromJSON(path)

  regs <- unique(witchtools::region_mappings[["witch17"]][["witch17"]])
  expect_setequal(names(j), c(regs, "world"))
})

test_that("premise_region_mapping converts iso3 to iso2 codes", {
  path <- withr::local_tempfile(fileext = ".json")

  witchtools::premise_region_mapping(filename = path)
  j <- jsonlite::fromJSON(path)

  expect_equal(j$usa, "US")
  expect_equal(j$brazil, "BR")
  expect_true(all(c("FR", "DE", "IT") %in% j$europe))
  # Kosovo is patched to the user-assigned XK code
  expect_true("XK" %in% j$europe)
  expect_false(any(is.na(unlist(j))))

  # the number of codes matches the number of countries in the mapping
  rm <- witchtools::region_mappings[["witch17"]]
  expect_equal(length(unlist(j)) - 2L, nrow(rm))
})

test_that("premise_region_mapping adds a world region", {
  path <- withr::local_tempfile(fileext = ".json")

  witchtools::premise_region_mapping(filename = path)
  j <- jsonlite::fromJSON(path)

  expect_equal(j$world, c("GLO", "RoW"))
})

test_that("premise_region_mapping prints to the console without a filename", {
  withr::local_dir(withr::local_tempdir())

  expect_output(witchtools::premise_region_mapping(), "\"usa\"")
  expect_length(list.files(), 0L)
})

test_that("premise_region_mapping works for another region mapping", {
  path <- withr::local_tempfile(fileext = ".json")

  witchtools::premise_region_mapping(n = "r5", filename = path)
  j <- jsonlite::fromJSON(path)

  regs <- unique(witchtools::region_mappings[["r5"]][["r5"]])
  expect_setequal(names(j), c(regs, "world"))
  expect_equal(j$world, c("GLO", "RoW"))
})

test_that("premise_region_mapping errors on an unknown region mapping", {
  withr::local_dir(withr::local_tempdir())

  expect_error(witchtools::premise_region_mapping(n = "not_a_mapping"))
  expect_length(list.files(), 0L)
})
