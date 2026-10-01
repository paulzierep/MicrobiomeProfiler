test_that("Galaxy downloads are persisted and uploaded asynchronously", {
  output_dir <- local_tempdir()
  bin_dir <- local_tempdir()
  put_args <- tempfile()
  put <- file.path(bin_dir, "put")
  writeLines(c("#!/bin/sh", "printf '%s\\n' \"$@\" > \"$MICROBIOMEPROFILER_PUT_ARGS\""), put)
  Sys.chmod(put, "0755")
  withr::local_envvar(c(
    MICROBIOMEPROFILER_OUTPUT_DIR = output_dir,
    MICROBIOMEPROFILER_PUT_ARGS = put_args,
    HISTORY_ID = "history-id",
    API_KEY = "secret-key",
    PATH = paste(bin_dir, Sys.getenv("PATH"), sep = .Platform$path.sep)
  ))

  source_file <- tempfile(fileext = ".txt")
  writeLines("result", source_file)
  output_path <- file.path(output_dir, "result.txt")
  file.copy(source_file, output_path)
  expect_true(galaxy_put_async(output_path))

  for (i in seq_len(50L)) {
    if (file.exists(put_args)) break
    Sys.sleep(0.1)
  }
  expect_equal(readLines(put_args), c("-p", output_path))
  log <- paste(readLines(file.path(output_dir, "galaxy_upload.log")), collapse = "\n")
  expect_false(grepl("secret-key", log, fixed = TRUE))
})
