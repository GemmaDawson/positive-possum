#' Function to create key for timecapture data.frame
#'
#' This function takes as input the employees short name and the time entry date
#'
#' @param shortName the users short name
#' @param TimeEntryDate the range of dates
#'
#' @return this function returns the redis key for the timecapture data.frame
#'
#' @export
create_timecapture_key <- function(shortName, TimeEntryDate){
  return(paste(shortName, as.character(TimeEntryDate), "timecapture", sep = ":"))
}