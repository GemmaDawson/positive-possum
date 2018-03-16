#' Function to create a range (inclusive) of dates
#'
#' This function takes as input the host ip and the port so it can connect to redis
#'
#' @param start_date the start date
#' @param end_date the end date
#'
#' @return this function returns a list of dates in increasing order
#'
#' @export
# peterf: this function returns a range of dates given a start and end date
makeDateRange <- function(start_date = as.Date("2017-04-02"), end_date = as.Date("2017-06-02")){
  if(start_date > end_date){
    switch_date = start_date
    start_date = end_date
    end_date = switch_date
  }
  return(seq(start_date, end_date, by = "+1 day"))
}
