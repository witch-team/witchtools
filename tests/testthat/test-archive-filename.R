
test_that("archive_filename builds data_<reg>_<time>_<commit>_<date>.zip", {
  skip_if(unname(Sys.which("git")) == "", "git not available")
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  system2("git", c("init", "-q", "."), stdout = FALSE, stderr = FALSE)
  system2("git", c("-c", "user.name=t", "-c", "user.email=t@t",
                   "commit", "-q", "--allow-empty", "-m", "init"),
          stdout = FALSE, stderr = FALSE)

  fn <- suppressWarnings(archive_filename("witch17", "t30"))

  expect_match(fn, "^data_witch17_t30_[0-9a-f]{6}_[0-9]{8}-[0-9]{4}\\.zip$")
})

test_that("archive_filename honours a custom extension", {
  skip_if(unname(Sys.which("git")) == "", "git not available")
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  system2("git", c("init", "-q", "."), stdout = FALSE, stderr = FALSE)
  system2("git", c("-c", "user.name=t", "-c", "user.email=t@t",
                   "commit", "-q", "--allow-empty", "-m", "init"),
          stdout = FALSE, stderr = FALSE)

  fn <- suppressWarnings(archive_filename("ed58", "t30", ext = ".tar.gz"))

  expect_match(fn, "^data_ed58_t30_[0-9a-f]{6}_[0-9]{8}-[0-9]{4}\\.tar\\.gz$")
})

test_that("archive_filename is deprecated", {
  skip_if(unname(Sys.which("git")) == "", "git not available")
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  system2("git", c("init", "-q", "."), stdout = FALSE, stderr = FALSE)
  system2("git", c("-c", "user.name=t", "-c", "user.email=t@t",
                   "commit", "-q", "--allow-empty", "-m", "init"),
          stdout = FALSE, stderr = FALSE)

  expect_warning(archive_filename("witch17", "t30"), "deprecated")
})
