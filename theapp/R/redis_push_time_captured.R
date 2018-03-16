#' Function to push a new time data.frame to redis
#'
#' This function takes as input the host ip the port and the query
#'
#' @param updatedDF the data.frame after updates have been made
#'
#' @return this function returns nothing
#'
#' @export

push_time_captured <- function(updatedDF){
  assertthat::assert_that("shortName" %in% names(updatedDF))
  assertthat::assert_that("EmployeeName" %in% names(updatedDF))
  assertthat::assert_that("TimeCaptureDate" %in% names(updatedDF))
  assertthat::assert_that("ProjectName" %in% names(updatedDF))
  assertthat::assert_that("TimeEntryDate" %in% names(updatedDF))
  assertthat::assert_that("TotalHours" %in% names(updatedDF))
  assertthat::assert_that("Comment" %in% names(updatedDF))
  assertthat::assert_that("Expense" %in% names(updatedDF))
  assertthat::assert_that("Modified" %in% names(updatedDF))
  assertthat::assert_that("Source" %in% names(updatedDF))
  assertthat::assert_that("New" %in% names(updatedDF))
  assertthat::assert_that("TimeLiveTask" %in% names(updatedDF))
  assertthat::assert_that("TaskName" %in% names(updatedDF))
  assertthat::assert_that("Tag" %in% names(updatedDF))
  assertthat::assert_that("LeaveType" %in% names(updatedDF))
  assertthat::assert_that("CompanyName" %in% names(updatedDF))
  assertthat::assert_that("TimeLiveProjectName" %in% names(updatedDF))
  key_ <- paste(updatedDF$shortName[1], as.character(updatedDF$TimeEntryDate[1]), "timecapture", sep = ":")
  redisSet(key = key_, value = updatedDF)
}
