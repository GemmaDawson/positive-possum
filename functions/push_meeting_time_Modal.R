# PUSH MEETING DUR TO TIME ENTRY PROMPT
push_meeting_time_Modal <- function(session, meetingproject, meetingpresent, meetingdur) {
  
  ns <- session$ns
  
  modalDialog(
    
    title = "ADD TIME ENTRY",
    
    dateInput(inputId = ns("meeting.entry.date"), label = "DATE", 
              value = Sys.Date(), startview = "month", weekstart = 1),
    
    selectInput(inputId = ns("meeting.entry.project"), 
                label = "Project",
                choices = list(`Meeting` = list(meetingproject),
                               `Change to Project:` = sort(unique(pivot.client.project$ProjectName[pivot.client.project$ProjectName != meetingproject])))),
    
    selectInput(inputId = ns("meeting.entry.task"), label = "TASK",
                choices = c(TASK = "", user.tasks),
                selected = "meeting"),
    
    numericInput(inputId = ns("meeting.dur.hours"), label = "hours",
                 value = as.numeric(unlist(strsplit(meetingdur, split = ":"))[1]),
                 min = 00, max = 23, step = 1),
    numericInput(inputId = ns("meeting.dur.mins"), label = "min",
                 value = as.numeric(unlist(strsplit(meetingdur, split = ":"))[2]),
                 min = 00, max = 59, step = 1),
  
    textInput(inputId = ns("meeting.entry.comment"), 
              label = "Comment", placeholder = "COMMENT"),
    
    hr(),
    checkboxGroupInput(inputId = ns("meeting.entry.present"), 
                       label = "Push time entries to:", 
                       choices = meetingpresent, 
                       selected = meetingpresent, inline = T),
  
    easyClose = F,
    
    footer = tagList(
      actionButton(ns("push.meeting.time"), "SAVE", width = "75px"),
      modalButton("CANCEL")
    )
    
  )
}
