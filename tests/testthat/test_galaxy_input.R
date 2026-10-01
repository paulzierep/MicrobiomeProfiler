test_that("Galaxy identifier files preserve textual IDs", {
  path <- tempfile()
  writeLines(c("001591", "853,39491", "# comment", ""), path)
  expect_equal(galaxy_identifier_input(path), "001591\n853\n39491")
  expect_null(galaxy_identifier_input(tempfile()))
})
