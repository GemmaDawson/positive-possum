context("Test function hello")


test_that("test output for various inputs", {
  
  # expect_match(object = hello("1"), regexp = "Error")
  expect_equal(object = hello("dfahsth"), expected = "Hello, dfahsth!")
  
})