#' Function to create a range of dates
#'
#' This function takes as input the host ip and the port so it can connect to redis
#'
#' @param period the number of days back for which a date range is required
#'
#' @return this function returns a list of dates in increasing order
#'
#' @export

firstDateRange <- function(period = 28){
  start_date = Sys.Date() - period
  end_date = Sys.Date()
  return(seq(start_date, end_date, by = "+1 day"))
}
