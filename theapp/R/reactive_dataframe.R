create_reactive_timecapture_module_df <- function(df.original, session){
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
  if(nrow(df.original) < 1){
    df.original <- dframeInit
  }
  df.date <- df.original %>%
    mutate(Date = lubridate::date(TimeEntryDate),
           Expense = ifelse(Expense == 0, "No", "Yes"),
           Duration = as.character(duration_to_hhmm(TotalHours))) %>%
    select(Date, Project = ProjectName,
           Task = TaskName,
           Duration, Comment)
  df.date <- df.date
  return(df.date)
}
