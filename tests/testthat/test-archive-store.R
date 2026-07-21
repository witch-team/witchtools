
test_that("archive_store copies the file into every existing directory", {
  base <- withr::local_tempdir()
  arch <- fs::path(base, "data.zip")
  writeLines("payload", arch)
  d1 <- fs::dir_create(fs::path(base, "d1"))
  d2 <- fs::dir_create(fs::path(base, "d2"))

  suppressWarnings(
    expect_output(witchtools::archive_store(arch, c(d1, d2)), "Copied")
  )

  expect_true(fs::file_exists(fs::path(d1, "data.zip")))
  expect_true(fs::file_exists(fs::path(d2, "data.zip")))
  expect_equal(readLines(fs::path(d1, "data.zip")), "payload")
  expect_equal(readLines(fs::path(d2, "data.zip")), "payload")
})

test_that("archive_store silently skips directories that do not exist", {
  base <- withr::local_tempdir()
  arch <- fs::path(base, "data.zip")
  writeLines("payload", arch)
  d1 <- fs::dir_create(fs::path(base, "d1"))
  missing <- fs::path(base, "nowhere")

  suppressWarnings(
    suppressMessages(res <- witchtools::archive_store(arch, c(missing, d1)))
  )

  expect_null(res)
  expect_true(fs::file_exists(fs::path(d1, "data.zip")))
  expect_false(fs::dir_exists(missing))
})

test_that("archive_store overwrites a pre-existing archive at destination", {
  base <- withr::local_tempdir()
  arch <- fs::path(base, "data.zip")
  writeLines("new content", arch)
  d1 <- fs::dir_create(fs::path(base, "d1"))
  writeLines("old content", fs::path(d1, "data.zip"))

  suppressWarnings(
    suppressMessages(witchtools::archive_store(arch, d1))
  )

  expect_equal(readLines(fs::path(d1, "data.zip")), "new content")
})

test_that("archive_store also works for directories containing spaces", {
  base <- withr::local_tempdir()
  arch <- fs::path(base, "data.zip")
  writeLines("payload", arch)
  d1 <- fs::dir_create(fs::path(base, "with space"))

  suppressWarnings(
    suppressMessages(witchtools::archive_store(arch, d1))
  )

  expect_true(fs::file_exists(fs::path(d1, "data.zip")))
  expect_equal(readLines(fs::path(d1, "data.zip")), "payload")
})

test_that("archive_store is deprecated", {
  base <- withr::local_tempdir()
  arch <- fs::path(base, "data.zip")
  writeLines("payload", arch)

  expect_warning(
    witchtools::archive_store(arch, fs::path(base, "nowhere")),
    "deprecated"
  )
})
