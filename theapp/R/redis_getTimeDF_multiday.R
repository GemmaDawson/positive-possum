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
getTimeDF_multiday <- function(conf, input = query){
  # make cluster -------------------------------------
  start_local_cluster <- function(){
    p_load(foreach)
    p_load(doParallel)
    # Calculate the number of cores
    # no_cores <- detectCores() - 1
    no_cores <- 1
    cl<-makeCluster(no_cores)
    registerDoParallel(cl)
  }
  stop_local_cluster <- function(){
    stopImplicitCluster()
  }
  # redisGetThis <- function(host = host_, port = port_, inputQuery){
  #   redisConnect(host, port)
  #   return(redisGet(inputQuery))
  # }
  start_local_cluster()
  ##################
  dframeCall <- foreach(index = 1:length(input)
                        ,.combine = rbind
                        ,.packages = c("rredis", "theapp")
  ) %dopar%
    redisGetThis(conf, key_ = input[index])
  ##################
  stop_local_cluster()
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
  if(is.null(dframeCall)){
    dframe <- dframeInit
  } else {
    dframe <- dframeCall
  }
  return(dframe)
}
