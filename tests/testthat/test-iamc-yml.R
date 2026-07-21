
test_that("iamc_native_regions_yml writes a yaml in the working directory", {
  withr::local_dir(withr::local_tempdir())

  witchtools::iamc_native_regions_yml()

  expect_true(file.exists("native_regions_WITCH_5_0.yml"))
  y <- yaml::read_yaml("native_regions_WITCH_5_0.yml")
  expect_length(y, 1L)
  expect_named(y[[1]], "WITCH 5.0")
})

test_that("iamc_native_regions_yml lists every native region with its countries", {
  path <- withr::local_tempfile(fileext = ".yml")

  witchtools::iamc_native_regions_yml(filename = path)
  y <- yaml::read_yaml(path)
  regs <- y[[1]][["WITCH 5.0"]]

  descs <- unique(witchtools::region_descriptions[["witch17"]]$description)
  expect_length(regs, length(descs))
  expect_setequal(vapply(regs, names, character(1)),
                  paste("WITCH 5.0", descs, sep = "|"))

  # each entry holds a `countries` list of country names
  entry <- Filter(function(x) names(x) == "WITCH 5.0|Brazil", regs)[[1]]
  expect_named(entry[[1]], "countries")
  expect_equal(entry[[1]]$countries, "Brazil")

  usa <- Filter(function(x) {
    names(x) == "WITCH 5.0|United States of America"
  }, regs)[[1]]
  expect_equal(usa[[1]]$countries, "United States")

  eur <- Filter(function(x) {
    grepl("^WITCH 5\\.0\\|Europe", names(x))
  }, regs)[[1]]
  expect_true(all(c("France", "Germany", "Italy") %in% eur[[1]]$countries))
})

test_that("iamc_native_regions_yml honours model and region mapping", {
  withr::local_dir(withr::local_tempdir())

  witchtools::iamc_native_regions_yml(model = "WITCH 6.1", n = "r5")

  expect_true(file.exists("native_regions_WITCH_6_1.yml"))
  y <- yaml::read_yaml("native_regions_WITCH_6_1.yml")
  expect_named(y[[1]], "WITCH 6.1")
  expect_length(y[[1]][["WITCH 6.1"]],
                length(unique(witchtools::region_descriptions[["r5"]]$description)))
  expect_true(all(grepl("^WITCH 6\\.1\\|",
                        vapply(y[[1]][["WITCH 6.1"]], names, character(1)))))
})

test_that("iamc_native_regions_yml errors on an unknown region mapping", {
  withr::local_dir(withr::local_tempdir())

  expect_error(witchtools::iamc_native_regions_yml(n = "not_a_mapping"))
  expect_length(list.files(), 0L)
})

test_that("iamc_region_mappings_yml writes a yaml in the working directory", {
  withr::local_dir(withr::local_tempdir())

  witchtools::iamc_region_mappings_yml()

  expect_true(file.exists("region_mappings_WITCH_5_0.yml"))
  y <- yaml::read_yaml("region_mappings_WITCH_5_0.yml")
  expect_named(y, c("model", "native_regions", "common_regions"))
  expect_equal(as.character(unlist(y$model)), "WITCH 5.0")
})

test_that("iamc_region_mappings_yml maps native regions to their long names", {
  path <- withr::local_tempfile(fileext = ".yml")

  witchtools::iamc_region_mappings_yml(filename = path)
  y <- yaml::read_yaml(path)

  regs <- witchtools::region_mappings[["witch17"]][["witch17"]]
  expect_setequal(vapply(y$native_regions, names, character(1)), unique(regs))

  usa <- Filter(function(x) names(x) == "usa", y$native_regions)[[1]]
  expect_equal(usa$usa, "WITCH 5.0|United States of America")
})

test_that("iamc_region_mappings_yml aggregates the requested common regions", {
  path <- withr::local_tempfile(fileext = ".yml")

  witchtools::iamc_region_mappings_yml(filename = path)
  y <- yaml::read_yaml(path)
  cnames <- vapply(y$common_regions, names, character(1))

  expect_true("World" %in% cnames)
  world <- Filter(function(x) names(x) == "World", y$common_regions)[[1]]
  expect_setequal(unlist(world$World),
                  unique(witchtools::region_mappings[["witch17"]][["witch17"]]))

  expect_true(any(grepl("\\(R5\\)$", cnames)))
  expect_true(any(grepl("\\(R9\\)$", cnames)))
  expect_true(any(grepl("\\(R10\\)$", cnames)))

  # every native region is assigned exactly once within each common mapping
  regs <- unique(witchtools::region_mappings[["witch17"]][["witch17"]])
  for (suffix in c("R5", "R9", "R10")) {
    sel <- grepl(paste0("\\(", suffix, "\\)$"), cnames)
    expect_setequal(unlist(lapply(y$common_regions[sel], function(x) x[[1]])),
                    regs)
  }
})

test_that("iamc_region_mappings_yml can restrict common regions to world", {
  path <- withr::local_tempfile(fileext = ".yml")

  witchtools::iamc_region_mappings_yml(filename = path, comm_regs = "world")
  y <- yaml::read_yaml(path)

  expect_length(y$common_regions, 1L)
  expect_named(y$common_regions[[1]], "World")
})

test_that("iamc_region_mappings_yml warns on an undefined common region", {
  path <- withr::local_tempfile(fileext = ".yml")

  expect_warning(
    witchtools::iamc_region_mappings_yml(filename = path,
                                         comm_regs = c("world", "nope")),
    "not defined in region_mappings"
  )

  y <- yaml::read_yaml(path)
  expect_length(y$common_regions, 1L)
})

test_that("iamc_region_mappings_yml errors on an unknown region mapping", {
  withr::local_dir(withr::local_tempdir())

  expect_error(witchtools::iamc_region_mappings_yml(n = "not_a_mapping"))
  expect_length(list.files(), 0L)
})
