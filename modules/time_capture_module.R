time_capture_module_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    box(title = "TIME CAPTURE", status = "primary", solidHeader = TRUE, width = 12,
        # date select // display // save & edit buttons
        fluidRow(
          column(12, textOutput(ns("selected.date"), tags$h1),
                 dateInput(inputId = ns("date"), label = "Select a date:",
                           value = Sys.Date(), startview = "month",
                           weekstart = 1, width = "100px"))
          # ,column(1, actionButton(inputId = ns("save.time.button"), label = "SAVE", width = "100px"))
          # ,column(1, actionButton(inputId = ns("edit.button"), label = "EDIT", width = "100px"))
          # ,column(1, actionButton(inputId = ns("delete.button"), label = "DELETE", width = "100px"))
          ),
        hr(),
        
        # time entry input // add time entry button
        fluidRow(
          column(3, selectInput(inputId = ns("project"), label = '', 
                                choices = '')),
          column(2, selectInput(inputId = ns("task"), label = "", 
                                choices = c(TASK = "", user.tasks))),
          # column(1, timeInput(inputId = "duration", label = "", value = c("00:00"))), 
          column(2,   
                 div(style="display: inline-block;vertical-align:top; width: 100px;",  
                     numericInput(inputId = ns("dur.hours"), label = "hours", value = 00,   
                                  min = 00, max = 23, step = 1)),  
                 div(style="display: inline-block;vertical-align:top; width: 100px;",  
                     numericInput(inputId = ns("dur.mins"), label = "min", value = 00,   
                                  min = 00, max = 59, step = 1))), 
          column(4, textInput(inputId = ns("comment"), label = "", placeholder = "COMMENT")),
          column(1, actionButton(inputId = ns("add.time.entry"), label = "ADD", width = "100px")
                 
                 )),
        
        # DT with selected date's time entries
        fluidRow(column(12, DTOutput(ns("time.data"))))
        ), 
    
    # Edit time entry
    uiOutput(outputId = ns("edit.time.entry"))
    
    # # Delete time entry
    # uiOutput(outputId = ns("delete.time.entry"))
    
    # uiOutput(outputId = ns("delete.time.entry")),
    
    # box(title = "LEAVE", status = "primary", solidHeader = TRUE, width = 12,
    #     fluidRow(
    #       column(12, rHandsontableOutput(ns("leave.data")))
    #       )
    #     )
  )
}


time_capture_module <- function(input, output, session) {

  userDF_ <- getUserDF(conf = config$rediscontainer, userName)
  # isAdmin <- reactive({
  #   if (userDF_$Admin){
  #     return(TRUE)
  #   } else {
  #     return(FALSE)
  #   }
  # })
  
  pivot.client.project <- get_user_projects(conf = config$rediscontainer, userDF = userDF_ )
  user.projects  <- pivot.client.project$ProjectName[pivot.client.project$EmployeeName == userDF_$EmployeeName & pivot.client.project$UserActive==1]
  
  updateSelectInput(session, inputId = "project", choices = c(PROJECT = '', user.projects))

  values <- reactiveValues(df.date = NULL
                           ,df.original = NULL
                           )
  
  # create bold "Day, Date" for top of page
  selected.date <- reactive({
    str_c(as.character(wday(input$date, label = T, abbr = F)), ", ", 
          mday(input$date), " ", 
          month(input$date, label = T, abbr = F))
  })
  
  output$selected.date <- renderText(selected.date())
  
  
  # list all options that need to passed to create pretty DataTables
  # this affects all DataTables on the page
  options(DT.options = list(dom = 't', filter = 'none'))
  
  observeEvent(input$date, {
    values$df.original <- getTimeDF(conf = config$rediscontainer, inputQuery_ = buildTimeQuery(userNameShort = userDF_$shortName, date = input$date))
    values$df.date <- create_reactive_timecapture_module_df(values$df.original, session)
    
    observeEvent(input$add.time.entry, {
      values$df.original <- getTimeDF(conf = config$rediscontainer, inputQuery_ = buildTimeQuery(userNameShort = userDF_$shortName, date = input$date))
      values$df.date <- create_reactive_timecapture_module_df(values$df.original, session)
    })
    observeEvent(input$edit_button, {
      values$df.original <- getTimeDF(conf = config$rediscontainer, inputQuery_ = buildTimeQuery(userNameShort = userDF_$shortName, date = input$date))
      values$df.date <- create_reactive_timecapture_module_df(values$df.original, session)
    })
    observeEvent(input$delete_button, {
      values$df.original <- getTimeDF(conf = config$rediscontainer, inputQuery_ = buildTimeQuery(userNameShort = userDF_$shortName, date = input$date))
      values$df.date <- create_reactive_timecapture_module_df(values$df.original, session)
    })
  })

  output$time.data <- DT::renderDataTable(server = FALSE, {
                                               datatable(
                                                 values$df.date %>% 
                                                   mutate(
                                                     Edit = edit_buttons(actionButton, nrow(values$df.date), id= 'edit_button_', label = "", icon=icon("pencil")
                                                                         ,onclick= sprintf('Shiny.onInputChange("%s",  this.id)', session$ns("edit_button"))),
                                                     Delete = delete_buttons(actionButton, nrow(values$df.date), id= 'delete_button_', label = "", icon=icon("trash")
                                                                             #bernie: thank goodness for Joe Cheng, #https://groups.google.com/forum/#!topic/shiny-discuss/lEgYL-5rPC8
                                                                             ,onclick= sprintf('Shiny.onInputChange("%s",  this.id)', session$ns("delete_button")))
                                                     
                                                   ),
                                                 rownames = FALSE, selection = 'single',
                                                 escape = F #bernie: need this to show delete_button
                                                 , options=list(columnDefs = list(list(visible=FALSE, targets=0)))
                                               )
                                             })
  
  # output$route_map_DT <- DT::renderDataTable(server = FALSE, {
  #   if(is.null(route()$stops)){
  #     
  #   }else{
  #     datatable(
  #       route()$stops %>% 
  #         filter(seq!=0) %>% 
  #         select(Name,  
  #                Duration=DurationInMinutes,
  #                ArrivalTimes,
  #                DepartureTimes
  #         ) %>% 
  #         mutate(up =DT_rows_buttons(actionButton, nrow(route()$stops)-2, 'button_', label = "↑", onclick = 'Shiny.onInputChange(\"up_button\",  this.id)' )
  #                ,down=DT_rows_buttons(actionButton, nrow(route()$stops)-2, 'button_', label = "↓", onclick = 'Shiny.onInputChange(\"down_button\",  this.id)')
  #                ,Name=sub('(^\\w+)\\s.+','\\1', Name) #bernie extract first word 
  #                ,Times = paste0(substr(chron::times(ArrivalTimes/24), 1, 5)
  #                                ,"-"
  #                                ,substr(chron::times(DepartureTimes/24),1, 5)
  #                )
  #         ) %>% 
  #         mutate(
  #           up=ifelse(row_number()==1, "",up)
  #           ,down=ifelse(row_number()==(nrow(route()$stops)-2), "",down)
  #         ) %>% 
  #         select(Name, up, down, Times, Duration)
  #       # ,extensions = 'RowReorder',
  #       # ,colnames = c(ID = 1)
  #       ,options = list(
  #         lengthChange = FALSE, 
  #         dom = 't',
  #         ordering=F,
  #         scrollX = TRUE#, 
  #         # rowReorder = TRUE
  #       )
  #       ,selection = "none"
  #       ,escape = FALSE
  #     )
  #   }
  #   
  # })
  
  # observeEvent(values$df.date, {
  #   browser()
  #   output$time.data <- DT::renderDataTable(values$df.date  %>%
  #                                             mutate(
  #                                               Edit = edit_buttons(actionButton, nrow(values$df.date), id= 'edit_button_', label = "", icon=icon("pencil")
  #                                                                      ,onclick= sprintf('Shiny.onInputChange("%s",  this.id)', session$ns("edit_button"))),
  #                                               Delete = delete_buttons(actionButton, nrow(values$df.date), id= 'delete_button_', label = "", icon=icon("trash")
  #                                                                      #bernie: thank goodness for Joe Cheng, #https://groups.google.com/forum/#!topic/shiny-discuss/lEgYL-5rPC8
  #                                                                      ,onclick= sprintf('Shiny.onInputChange("%s",  this.id)', session$ns("delete_button")))
  # 
  #                                             )
  #                                           , server = FALSE, rownames = FALSE, selection = 'single',
  #                                           escape = F #bernie: need this to show delete_button
  #                                           )
  #   
  # })
  
  observeEvent(input$delete_button, {
    selectedRow <- as.numeric(strsplit(input$delete_button, "_")[[1]][3])
    values$df.date <- values$df.date[rownames(values$df.date) != selectedRow, ]
    test_and_write_redis(input.df_ = values$df.date, EntryDate_ = input$date, user.dataframe = userDF_)
  })


  
#########################
# ADD TIME ENTRY
#########################
  # ADD NEW TIME ENTRY
  observeEvent(input$add.time.entry, {
    req(input$date)
    req(input$project)
    req(input$task)

    
    updateSelectInput(session = session, inputId = "project", 
                      choices = c(PROJECT = '', user.projects))
    updateSelectInput(session = session, inputId = "task",
                      choices = c(TASK = "", user.tasks))
    updateNumericInput(session = session, inputId = "dur.hours",
                       value = 0)
    updateNumericInput(session = session, inputId = "dur.mins",
                       value = 0)
    updateTextInput(session = session, inputId = "comment",
                    placeholder = "COMMENT", value = "")


    values$df.date <- add_row(values$df.date, Date = input$date,
                                   Project = input$project,
                                   Task = input$task,
                                   Duration = paste0(sprintf("%02d", input$dur.hours),
                                                     ":", sprintf("%02d", input$dur.mins)),
                                   Comment = input$comment
                              )
    
    test_and_write_redis(input.df_ = values$df.date, EntryDate_ = input$date, user.dataframe = userDF_)
  })
    

  
  # # SAVE SELECTED DATE'S TIME ENTRY
  # observeEvent(input$save.time.button, {
  #   cat("Write this thing to disk\n")
  #   write_rds(x = values$df.original, path = "data/pivot.timedf1.rds")
  #   write_rds(x = values$df.date, path = "data/pivot.timedf2.rds")
  #   
  # })

#########################
# EDIT TIME ENTRY
#########################
  # EDIT SELECTED ROW TIME ENTRY
  # DISPLAY EDIT BOX WITH SELECTED ROW'S DATA
  # observeEvent(input$edit.button, {
  
  observeEvent(input$edit_cancel,{
    hide("edit.time.entry")
    values$df.original <- getTimeDF(conf = config$rediscontainer, inputQuery_ = buildTimeQuery(userNameShort = userDF_$shortName, date = input$date))
    values$df.date <- create_reactive_timecapture_module_df(values$df.original, session)
  })
  
  
  observeEvent(input$edit_button, {
    selectedRow <- as.numeric(strsplit(input$edit_button, "_")[[1]][3])
    
    show("edit.time.entry")

    output$edit.time.entry <- renderUI({
      
      ns <- session$ns
      
      box(title = "EDIT ENTRY", status = "primary", solidHeader = TRUE, width = 12,
          
          fluidRow(
            column(1, offset = 11, actionButton(inputId = ns("edit_cancel"), label = "CANCEL", width = "100px"))
          ),

          fluidRow(
            column(1, dateInput(inputId = ns("edit.date"), label = '',
                                value = values$df.date[selectedRow, names(values$df.date) == "Date"],
                                startview = "month", weekstart = 1)),
            column(2, selectInput(inputId = ns("edit.project"), label = '',
                                  choices = c(PROJECT = '', user.projects),
                                  selected = values$df.date[selectedRow, names(values$df.date) == "Project"])),
            column(2, selectInput(inputId = ns("edit.task"), label = "",
                                  choices = c(TASK = "", user.tasks),
                                  selected = values$df.date[selectedRow, names(values$df.date) == "Task"])),
            column(2,
                   div(style="display: inline-block;vertical-align:top; width: 100px;",
                       numericInput(inputId = ns("edit.dur.hours"), label = "hours",
                                    value = as.numeric(unlist(strsplit(x = values$df.date[selectedRow, names(values$df.date) == "Duration"],
                                                                       split = ":"))[1]),
                                    min = 00, max = 23, step = 1)),
                   div(style="display: inline-block;vertical-align:top; width: 100px;",
                       numericInput(inputId = ns("edit.dur.mins"), label = "min",
                                    value = as.numeric(unlist(strsplit(x = values$df.date[selectedRow, names(values$df.date) == "Duration"],
                                                                       split = ":"))[2]),
                                    min = 00, max = 59, step = 1))),
            column(4, textInput(inputId = ns("edit.comment"), label = "", placeholder = "COMMENT",
                                value = values$df.date[selectedRow, names(values$df.date) == "Comment"])),
            column(1, actionButton(inputId = ns("save.time.entry"), label = "SAVE", width = "100px"))
          ))
      
      })
    
  })
  
  # SAVE CHANGES MADE TO SELECTED ROW
  observeEvent(input$save.time.entry, {
    if(!is.null(input$time.data_rows_selected)){
      values$df.date$Project <- as.character(values$df.date$Project)
      values$df.date$Task <- as.character(values$df.date$Task)
      values$df.date$Comment <- as.character(values$df.date$Comment)

      # update date
      values$df.date[input$time.data_rows_selected, names(values$df.date) == "Date"] <- input$edit.date
      # update project
      values$df.date[input$time.data_rows_selected, names(values$df.date) == "Project"] <- input$edit.project
      # update task
      values$df.date[input$time.data_rows_selected, names(values$df.date) == "Task"] <- input$edit.task
      # update duration
      values$df.date[input$time.data_rows_selected, names(values$df.date) == "Duration"] <- paste0(sprintf("%02d", input$edit.dur.hours), 
                                                                                                   ":", sprintf("%02d", input$edit.dur.mins))
      # update comment
      values$df.date[input$time.data_rows_selected, names(values$df.date) == "Comment"] <- input$edit.comment
      
      if(nrow(values$df.date %>% filter(Date != input$date)) > 0){
        test_and_write_redis(input.df_ = values$df.date %>% filter(Date != input$date), EntryDate_ = values$df.date$Date[input$time.data_rows_selected], user.dataframe = userDF_, append = TRUE)
        test_and_write_redis(input.df_ = values$df.date %>% filter(Date == input$date), EntryDate_ = input$date, user.dataframe = userDF_)
      } else {
        test_and_write_redis(input.df_ = values$df.date %>% filter(Date == input$date), EntryDate_ = input$date, user.dataframe = userDF_)
      }
    }
    hide("edit.time.entry")
    values$df.original <- getTimeDF(conf = config$rediscontainer, inputQuery_ = buildTimeQuery(userNameShort = userDF_$shortName, date = input$date))
    values$df.date <- create_reactive_timecapture_module_df(values$df.original, session)
  })
  
  
# #########################
# # DELETE TIME ENTRY
# #########################
# 
#   observeEvent(input$delete.button, {
# 
#     show("delete.time.entry")
#     hide("edit.time.entry")
# 
#     output$delete.time.entry <- renderUI({
# 
#       ns <- session$ns
# 
#       # show("delete.time.entry")
# 
#       output$to.be.deleted <-  renderDT(server = FALSE, rownames = FALSE,
#                                          selection = 'single',
#                                          {df2[input$time.data_rows_selected,]})
# 
#       box(title = "DELETE ENTRY", status = "primary", solidHeader = TRUE, width = 12,
# 
#           fluidRow(
#             column(1, offset = 10, actionButton(inputId = ns("really.delete"), label = "DELETE", width = "100px")),
#             column(1, actionButton(inputId = ns("dont.delete"), label = "CANCEL", width = "100px"))
#             ),
# 
#           fluidRow(
#             column(12, DTOutput(ns("to.be.deleted")))
#           )
#           )
# 
#     })
#   })
  
  # # DELETE ROW FROM DF
  # observeEvent(input$really.delete, {
  #   
  #   df2 <- df2[-input$time.data_rows_selected,]
  #   
  #   output$time.data <- renderDT(server = FALSE, rownames = FALSE,
  #                                selection = 'single',
  #                                { df2 })
  #   
  #   hide("delete.time.entry")
  # })
  # 
  # # NO!  DON'T DELETE!
  # observeEvent(input$dont.delete, {
  #   
  #   hide("delete.time.entry")
  # 
  # })
  
 
  # once confirmed, delete entry
  # observeEvent$
  

  
  # leave.df <- reactive({
  #   rhandsontable(pivot.time %>%
  #                    filter(EmployeeName == user,
  #                           TimeEntryDate == input$date,
  #                           !is.na(LeaveType)) %>%
  #                    mutate(Duration = as.character(duration_to_hhmm(TotalHours))) %>%
  #                    select(Date = TimeEntryDate,
  #                           Leave = LeaveType, 
  #                           Duration, Comment), height = 100) %>%
  #     hot_cols(colWidths = c(100, 200, 80, 650)) %>%
  #     hot_col(col = "Date", type = "date", 
  #             default = input$date, dateFormat = "YYYY-MM-DD") %>% 
  #     hot_col(col = "Leave", type =  "autocomplete", 
  #             source = leave.type, strict = T) %>%
  #     hot_col(col = "Duration", source = timeInput, halign = "htCenter") %>% 
  #     hot_col(col = "Comment", copyable = TRUE, language = 'en-GB') %>% 
  #     hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
  #   
  # })
  # 
  # output$leave.data <- renderRHandsontable(leave.df())
  # 
}
