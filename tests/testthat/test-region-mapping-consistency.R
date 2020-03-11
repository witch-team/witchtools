test_that("iso3 are consistent across region_mappings", {

  f <- Sys.glob(file.path(system.file("regions", package = "witchtools"),
                         "*.inc"))
  m <- lapply(f, load_region_mapping)

  print(length(unique(unlist(lapply(m, function(x)x$iso3)))))

  expect_equal(length(unique(unlist(lapply(m, function(x)x$iso3)))),
               unique(unlist(lapply(m,nrow))))

})
