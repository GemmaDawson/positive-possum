#' Function to build a single day time query
#'
#' This function takes as input the the users short name found in the userDF and the date range
#'
#' @param userNameShort the users short name
#' @param dates the range of dates
#'
#' @return this function returns a list of dates in increasing order
#'
#' @export
buildTimeQuery <- function(userNameShort = userDF$shortName[[1]], date = Sys.Date()){
  return(paste(userNameShort, date, "timecapture", sep = ":"))
}
