#' Function to return a time data.frame from redis
#'
#' This function takes as input the host ip the port and the query
#'
#' @param host_ the users short name
#' @param port_ the range of dates
#' @param input the range of dates
#'
#' @return this function returns the redis result
#'
#' @export

getTimeDF <- function(conf, inputQuery_){
  dframeCall <- redisGetThis(conf, key_ = inputQuery_)
  dframeInit <- data.frame(
    shortName=character(),
    EmployeeName=character(),
    TimeCaptureDate=as.Date(character()),
    ProjectName=character(),
    TimeEntryDate=as.Date(character()),
    TotalHours=numeric(),
    Comment=character(),
    Expense=numeric(),
    Modified=numeric(),
    Source=character(),
    New=numeric(),
    TimeLiveTask=numeric(),
    TaskName=character(),
    Tag=character(),
    LeaveType=character(),
    CompanyName=character(),
    TimeLiveProjectName=numeric()
  )
  if(is.null(dframeCall) || is.na(dframeCall$TimeEntryDate) || is.na(dframeCall$TotalHours) || (nrow(dframeCall) < 1)){
    dframe <- dframeInit
  } else {
    dframe <- dframeCall
  }
  return(dframe)
}
