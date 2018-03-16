home_module_UI <- function(id){
  
  ns <- NS(id)
  userDF_ <- getUserDF(conf = config$rediscontainer, userName)
  pivot.members <- redisGetThis(conf = config$rediscontainer, key_ = "user")
  tagList(
    # valueBox(value = userDF_$PreferredName, subtitle = userDF_$EmailAddress),
    infoBoxOutput(ns("cap.time")),
    hello(name = userDF_$PreferredName)
  )
}

home_module <- function(input, output, session){
  
  userDF_ <- getUserDF(conf = config$rediscontainer, userName)
  
  pivot.time <- getTimeDF_multiday(
    conf = config$rediscontainer, 
    input = buildTimeQuery_multiDay(
      userNameShort = userDF_$shortName, 
      dates = makeDateRange(
        start_date = floor_date(Sys.Date(), unit = "week", week_start = 1), 
        end_date = ceiling_date(Sys.Date(), unit = "week", week_start = 1) - 1
        )
      )
    )
  this.weeks.time <- pivot.time %>%
    filter(EmployeeName == userDF_$EmployeeName,
           TimeEntryDate >= floor_date(Sys.Date(), unit = "week", week_start = 1)) %>%
    summarise(total.time = sum(TotalHours)) %>% 
    pull()
  
  output$cap.time <- renderInfoBox({
    infoBox(title = "TIME CAPTURED THIS WEEK",
            value = h3(str_c(this.weeks.time, " hours")),
            icon = icon("hourglass-half"),
            color = "aqua",
            href = "www.pivotsciences.com",
            fill = T)
  })
  
  # callModule(admin_tab_module, "home.tab.admin")
  
  # callModule(personal_details_module, "home.personal.details")
  
}