meeting_tab_module_UI <- function(id) {
  
  ns <- NS(id)
  
  
  tagList(
    fluidRow(
      
      column(width = 4,
             
             box(title = "PROJECT TEAM", width = NULL, status = "primary", solidHeader = TRUE,
                 # choose the project to which this meeting is related
                 selectInput(inputId = ns("selected.project"), label = "Select a project:",
                             choices = '',
                             multiple = F, width = "400px"),
                 
                 # display check box list of team members
                 checkboxGroupInput(inputId = ns("project.team"), label = "TEAM"),
                 
                 # does the app need to generate a random order 
                 radioButtons(inputId = ns("list.needed"), label = "Generate random order?",
                              choices = c("Yes", "No"), inline = T, width = "300px",
                              selected = "Yes"),
                 
                 # create list (if required) and start timing the meeting
                 actionButton(inputId = ns("start.meeting"), label = "Start Meeting", 
                              width = "150px"),
                 
                 # add latecomers to random list/present
                 actionButton(inputId = ns("late"), label = "Add", width = "150px")
             )
             # , add_team_to_project_UI(ns("meeting.tab.add.to.project"))
      ),
      
      column(width = 4,
             box(title = "NOTES", width = NULL, status = "primary", solidHeader = TRUE,
                 actionButton(inputId = ns("finished.meeting"), label = "Finished", 
                              width = "150px"),
                 textOutput(ns("event.time.remaining"))
             )
      ),
      
      conditionalPanel(condition = sprintf("input['%s'] == 'Yes'", ns("list.needed")),
                       column(width = 4,
                              box(title = "RANDOM ORDER", width = NULL, status = "primary", solidHeader = TRUE,
                                  tags$h3(tags$ol(htmlOutput(ns("random.list"))))
                              ))
      )
    )
  )
}

meeting_tab_module <- function(input, output, session){
  
  values <- reactiveValues()
  
  # hide these buttons until needed
  hide("late")
  hide("finished.meeting")
  userDF_ <- getUserDF(conf = config$rediscontainer, userName)
  user.projects <- get_user_projects(conf = config$rediscontainer, userDF = userDF_) %>% pull(ProjectName)
  pivot.client.project <- get_all_projects(conf_ = config$rediscontainer)
  pivot.members <- redisGetThis(conf = config$rediscontainer, key_ = "user")
  
  updateSelectInput(session, inputId = "selected.project", choices = user.projects, selected = "PIVOT SCIENCES - HEAD OFFICE")
  team <- reactive({ as.data.frame(pivot.client.project %>% 
                                     filter(ProjectName == input$selected.project) %>% 
                                     left_join(pivot.members, by = "EmployeeName") %>% 
                                     filter(Current == T) %>% 
                                     ungroup() %>% 
                                     select(EmployeeName, PreferredName, UserActive) %>% 
                                     arrange(-UserActive))
  })
  
  
  # display active employees for the selected project & update project team
  observeEvent(input$selected.project, {
    
    # 
    updateCheckboxGroupInput(session = session, inputId = "project.team", 
                             choiceValues = team()$EmployeeName,
                             choiceNames = team()$PreferredName, 
                             selected = team()$EmployeeName[team()$UserActive == 1])
    
    updateRadioButtons(session = session, inputId = "list.needed", 
                       selected = ifelse(input$selected.project == "PIVOT SCIENCES - HEAD OFFICE",
                                         "Yes", "No"))
  })
  
  # We generally only use random order for weekly status meeting, but still 
  # available to be used for other meetings
  # ran.list <- eventReactive(input$start.meeting, {
  #   sample(left_join(input$project.team, pivot.members) , length(input$project.team), replace = F)
  #   })
  
  observeEvent({
    input$start.meeting
  }, {
    
  })
  
  
  # output$random.list <- renderUI(lapply(ran.list(), tags$li))
  
  observeEvent({
    input$start.meeting
  },{
    values$EventTime <- Sys.time()
    values$TotalTime <- NULL
    show("finished.meeting")
    show("event.time.remaining")
    hide("start.meeting")
    

    if(input$list.needed == "Yes"){
      
      list.names <- pivot.members %>% 
        filter(Current == T, 
               EmployeeName %in% input$project.team) %>% 
        select(PreferredName) %>% 
        pull(PreferredName)
      
      output$random.list <- renderUI(lapply(sample(list.names, length(list.names), replace = F), tags$li))
    }
    
  })
  

  output$event.time.remaining <- renderText({
    values$EventTime
    values$TotalTime
    if(!is.null(values$TotalTime)){
      paste("The duration of the meeting was:", 
            timeDisplay(values$TotalTime)
      )
    } else if(!is.null(values$EventTime)) {
      invalidateLater(1000, session)
      paste("The duration of the meeting is:", 
            timeDisplay(
              round(difftime(Sys.time(), values$EventTime, units='sec'))
            )
      )
    }
  })
  

  observeEvent(input$finished.meeting, {
    show("start.meeting")
    hide("finished.meeting")
    values$TotalTime <- round(difftime(Sys.time(), values$EventTime, units='sec'))
    values$EventTime <- NULL
    
    # # ?PUSH TIME ENTRY?
    # showModal(push_meeting_time_Modal(session, 
    #                                   meetingproject = input$selected.project, 
    #                                   meetingpresent = input$project.team, 
    #                                   meetingdur = duration_to_hhmm(as.numeric(ceiling((values$TotalTime/(60*60))/0.25)*0.25))
    #))
    
    
  })

  
  # observeEvent(input$push.meeting.time, {
  #   
  #   pivot.time <- add_row(pivot.time,
  #                         EmployeeName = input$meeting.entry.present,
  #                         ProjectName = input$meeting.entry.project,
  #                         TimeEntryDate = input$meeting.entry.date,
  #                         TimeCaptureDate = date(Sys.Date()),
  #                         TotalHours = (input$meeting.dur.hours + (input$meeting.dur.mins)/60),
  #                         Comment = input$meeting.entry.comment,
  #                         Expense = 0,
  #                         Modified = 0,
  #                         Source = "PivotTheApp_Meeting",
  #                         New = 1,
  #                         TimeLiveTask = NA,
  #                         TaskName = input$meeting.entry.task,
  #                         Tag = NA,
  #                         LeaveType = NA,
  #                         CompanyName = unique(pivot.client.project$CompanyName[pivot.client.project$ProjectName == input$meeting.entry.project]),
  #                         TimeLiveProjectName = NA)
  #   
  #   write_rds(pivot.time, paste0(getwd(), "/data/pivot.time.rds"))
  #   
  #   showModal(modalDialog(
  #     title = "Success",
  #     str_c("Time entries have been created for ", input$meeting.entry.project),
  #     footer = tagList(
  #       modalButton("Whoo!")
  #     )
  #   ))
  #   
  # })
  
  ###########################################################################################################
  # THE ADD LATECOMERS TO RANDOM LIST CURRENTLY DOES NOT WORK IN THE CORRECT WAY AND AS SUCH IS COMMENTED OUT
  # IT IS SUPPPOSED TO TAKE THE CURRENT LIST AND RANDOMLY ADD TO THE BOTTOM ANY LATECOMERS
  # CURRENTLY IT DOES THIS FOR THE FIRST ADD BUT THEN RANDOMISES ALL LATECOMERS
  # WHAT I NEED TO DO FIND A WAY OF UPDATING THE ran.list() WHEN THE FIRST SET OF LATECOMERS IS ADDED
  
  # if(length(ran.list()) != length(team())){
  #   show("late")
  #   }
  # })
  
  # observeEvent(input$late, {
  #   if(input$list.needed == "Yes"){
  #     
  #     current <- ran.list()
  #     
  #     present2 <- isolate(input$project.team)
  #     
  #     add.me <- present2[!(present2 %in% current)]
  #     
  #     new.list <- reactive({ append(current, sample(add.me, length(add.me), replace = F)) })
  #     
  #     ran.list <- reactive({ isolate(new.list()) })
  #     
  #     output$random.list <- renderUI(lapply(new.list(), tags$li))
  # 
  #   }
  # })
  ###########################################################################################################    
  # callModule(add_team_to_project, "meeting.tab.add.to.project")
}


























