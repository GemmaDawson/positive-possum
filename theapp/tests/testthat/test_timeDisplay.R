context("Test function timeDisplay")


test_that("test output for various inputs", {

  expect_match(object = timeDisplay(), regexp = "Error")
  expect_match(object = timeDisplay("1"), regexp = "Error")
  expect_equal(object = timeDisplay(round(difftime(Sys.time(), Sys.time() - 1, units='sec'))), expected = "1 secs")
  expect_equal(object = timeDisplay(round(difftime(Sys.time(), Sys.time() - 1*60, units='sec'))), expected = "1 mins 0 secs")
  expect_equal(object = timeDisplay(round(difftime(Sys.time(), Sys.time() - 1*60*60, units='sec'))), expected = "1 hr 0 mins 0 secs")

})


