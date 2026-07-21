
test_that("archive_restore finds the archive and unzips it in extract_dir", {
  base <- withr::local_tempdir()
  src <- fs::dir_create(fs::path(base, "src"))
  writeLines(c("a", "b"), fs::path(src, "content.txt"))
  store <- fs::dir_create(fs::path(base, "store"))
  arch <- fs::path(store, "data.zip")
  zip::zip(arch, "content.txt", root = src)

  out <- fs::dir_create(fs::path(base, "out"))
  suppressWarnings(
    expect_output(
      res <- witchtools::archive_restore("data.zip", store, extract_dir = out),
      "Found archived data"
    )
  )

  expect_equal(as.character(res), as.character(arch))
  expect_true(fs::file_exists(fs::path(out, "content.txt")))
  expect_equal(readLines(fs::path(out, "content.txt")), c("a", "b"))
})

test_that("archive_restore searches the directory list in order", {
  base <- withr::local_tempdir()
  src <- fs::dir_create(fs::path(base, "src"))
  d1 <- fs::dir_create(fs::path(base, "d1"))
  d2 <- fs::dir_create(fs::path(base, "d2"))

  writeLines("first", fs::path(src, "content.txt"))
  zip::zip(fs::path(d1, "data.zip"), "content.txt", root = src)
  writeLines("second", fs::path(src, "content.txt"))
  zip::zip(fs::path(d2, "data.zip"), "content.txt", root = src)

  out <- fs::dir_create(fs::path(base, "out"))
  suppressWarnings(
    suppressMessages(
      res <- witchtools::archive_restore("data.zip", c(d1, d2), extract_dir = out)
    )
  )

  expect_equal(as.character(res), as.character(fs::path(d1, "data.zip")))
  expect_equal(readLines(fs::path(out, "content.txt")), "first")
})

test_that("archive_restore skips missing directories and returns NULL if absent", {
  base <- withr::local_tempdir()
  out <- fs::dir_create(fs::path(base, "out"))

  suppressWarnings(
    suppressMessages(
      res <- witchtools::archive_restore(
        "data.zip",
        c(fs::path(base, "nowhere"), out),
        extract_dir = out
      )
    )
  )

  expect_null(res)
  expect_equal(fs::dir_ls(out), fs::dir_ls(out)[0])
})

test_that("archive_restore errors on a corrupted archive", {
  base <- withr::local_tempdir()
  store <- fs::dir_create(fs::path(base, "store"))
  writeLines("this is not a zip file", fs::path(store, "data.zip"))
  out <- fs::dir_create(fs::path(base, "out"))

  suppressWarnings(
    expect_error(
      suppressMessages(
        witchtools::archive_restore("data.zip", store, extract_dir = out)
      )
    )
  )
})

test_that("archive_restore is deprecated", {
  base <- withr::local_tempdir()
  expect_warning(
    witchtools::archive_restore("data.zip", fs::path(base, "nowhere"),
                                extract_dir = base),
    "deprecated"
  )
})
