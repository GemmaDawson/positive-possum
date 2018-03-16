#' Function to morph timecapture df to redis df
#'
#' This function takes the timecapture df and morphs it into a redis df
#'
#' @param df.input timecapture df
#' @param captureDate date that time is being captured against
#'
#' @return this returns the df in the redis format
#'
#' @export
# df.timecapture <- readRDS(file = path.expand(file.path(getwd(),"data","df.date.rds")))
# df.redis <- readRDS(file = path.expand(file.path(getwd(),"data","df.original.rds")))
# function takes df.timecapture df and  morphs it into the df.redis format
morph_to_redis_format_from_timecapture <- function(df.input = df.timecapture, captureDate = Sys.Date(), userDataframe){
  # browser()
  tryCatch(
    {
      if(TRUE){
        df.out <- df.input %>%
          mutate(
            shortName = userDataframe$shortName,
            EmployeeName = userDataframe$EmployeeName,
            TimeEntryDate = as.character(lubridate::date(Date)),
            TimeCaptureDate = as.character.Date(captureDate),
            Expense = 0L, # need to get expense from somewhere
            Modified = 0L, # need to decide if we want this?
            TotalHours = theapp::hhmm_to_duration(Duration),
            Tag = c(NA),
            LeaveType = c(NA),
            Source = "THEAPP",
            New = 0L, # again do we need this?
            TimeLiveTask = c(NA),
            TimeLiveProjectName = c(NA),
            CompanyName = "Pivot Sciences"
          ) %>%
          select(
            shortName,
            EmployeeName,
            TimeCaptureDate,
            ProjectName = Project,
            TimeEntryDate,
            TotalHours,
            Comment,
            Expense,
            Modified,
            Source,
            New,
            TimeLiveTask,
            TaskName = Task,
            Tag,
            LeaveType,
            CompanyName,
            TimeLiveProjectName
          )
        return(df.out)
      } else {
        return(FALSE)
        stop()
      }
    }, error = function(e){return(FALSE)}
  )
}

