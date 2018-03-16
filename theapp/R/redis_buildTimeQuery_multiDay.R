#' Function to build a multiday time query
#'
#' This function takes as input the the users short name found in the userDF and the date range
#'
#' @param userNameShort the users short name
#' @param dates the range of dates
#'
#' @return this function returns a list of dates in increasing order
#'
#' @export

buildTimeQuery_multiDay <- function(userNameShort = userDF$shortName[[1]], dates = firstDateRange()){
  return(paste(userNameShort, dates, "timecapture", sep = ":"))
}
