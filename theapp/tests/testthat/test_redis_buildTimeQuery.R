context("Test function redis_buildTimeQuery")


test_that("test output for various inputs", {

  # expect_match(object = buildTimeQuery(), regexp = "Error")
  expect_equal(object = buildTimeQuery("1"), expected = paste("1", Sys.Date(),"timecapture", sep = ":"))
  expect_equal(object = buildTimeQuery(userNameShort = "gemmad001", date = "2018-01-02"), expected = paste("gemmad001", "2018-01-02","timecapture", sep = ":"))

})
