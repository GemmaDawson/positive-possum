function(input, output, session){
  if(!is.null(session$user)){
    userName <<- session$user
  }
  if(isDev){
    userName <<- "Mickey.Mouse1@hotmail.com"
  }
  cat("The username is:",userName,"\n",file = stderr())
  
  callModule(home_module, "home.page")
  timecapturedatareturn <- callModule(time_capture_module, "time.capture")
  # timevalues <- reactiveValues()
  
  meeting_tabdatareturn <- callModule(meeting_tab_module, "meeting.data")
}

