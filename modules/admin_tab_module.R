admin_tab_module_UI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    fluidRow(
      tabBox(id = ns("administration.tabs"), title = "ADMIN", height = "200px",
             tabPanel(title = "PROJECT", 
                      selectInput(inputId = ns("select.project.admin"), 
                                  label = "",
                                  choices = list(`Select a Project` = '',
                                                 "New Project",
                                                 `Current Projects` = sort(unique(pivot.client.project$ProjectName)))),
                      actionButton(inputId = ns("project.admin.button"),
                                   label = "PROJECT INFO", width = "150px")),
             tabPanel(title = "EMPLOYEE", 
                      selectInput(inputId = ns("select.employee.admin"), 
                                  label = "",
                                  choices = list(`Select an Employee` = '',
                                                 "New Employee",
                                                 `Current Employees` = sort(pivot.members$EmployeeName[pivot.members$Current == T]),
                                                 `Former Employees` = sort(pivot.members$EmployeeName[pivot.members$Current == F]))),

                      actionButton(inputId = ns("employee.admin.button"),
                                   label = "EMPLOYEE INFO", width = "150px")),
             tabPanel(title = "DELETE",
                      
                      selectInput(inputId = ns("remove.employee.project"), 
                                  label = "Remove Employee from Project",
                                  choices = c("NOT AVAILABLE")),
                      actionButton(inputId = ns("remove.employee.button"),
                                   label = "REMOVE", width = "150px"),
                      hr(),
                      selectInput(inputId = ns("delete.project"), 
                                  label = "Delete a Project Record",
                                  choices = c("NOT AVAILABLE")),
                      actionButton(inputId = ns("delete.project.button"),
                                   label = "DELETE", width = "150px"),
                      hr(),
                      selectInput(inputId = ns("delete.employee"), 
                                  label = "Delete an Employee Record",
                                  choices = c("NOT AVAILABLE")),
                      actionButton(inputId = ns("delete.employee.button"),
                                   label = "DELETE", width = "150px"))
             )
      )
  )
}

admin_tab_module <- function(input, output, session) {

###### PROJECT ADMIN
  # display modal for new project info
  observeEvent(input$project.admin.button, {
    
    req(input$select.project.admin)
    
    if(input$select.project.admin == "New Project") {
      showModal(new_project_Modal(session))
    } else {

      showModal(edit_project_Modal(session, 
                                   project = input$select.project.admin,
                                   active.team = pivot.client.project$EmployeeName[pivot.client.project$ProjectName == input$select.project.admin 
                                                                                   & pivot.client.project$UserActive == 1],
                                   inactive.team = pivot.client.project$EmployeeName[pivot.client.project$ProjectName == input$select.project.admin 
                                                                                   & pivot.client.project$UserActive == 0]))
    }
  })
  
###### NEW PROJECT
  # start new project name with client's name
  observeEvent(input$pivot.client, {
    
    req(input$pivot.client)
    
      client.name <- paste(toupper(input$pivot.client), "- ")
      updateTextInput(session = session, inputId = "project.name",
                    value = client.name)
    
    })
  
  # if it's a new client, enter name
  observeEvent(input$new.pivot.client, {
    
    req(input$pivot.client)
    
    client.name <- paste(toupper(input$new.pivot.client), "- ")
    updateTextInput(session = session, inputId = "project.name",
                    value = client.name)
  })
  
  # add new project to pivot.client.project.rds
  observeEvent(input$save.project, {
    
    req(input$new.project.team, input$project.name)
    req(!(input$project.name %in% pivot.client.project$ProjectName))
    
    client.name <- ifelse(test = input$pivot.client == "New Client", 
                          yes = input$new.pivot.client,
                          no = input$pivot.client)

    pivot.client.project <- add_row(as.data.frame(pivot.client.project),
                                    CompanyName = client.name,
                                    ProjectName = toupper(input$project.name),
                                    EmployeeName = input$new.project.team,
                                    UserActive = 1)

    write_rds(pivot.client.project, paste0(getwd(), "/data/pivot.client.project.rds"))
    
    updateSelectInput(session = session, inputId = "select.project.admin", 
                      selected = "`Select an Employee` = ''")
    
    showModal(modalDialog(
      title = "Well done!",
      paste0("You've created the project ", toupper(input$project.name)),
      footer = tagList(
        modalButton("Awesome!")
      )
    ))
    
  })
  

###### EDIT PROJECT
  observeEvent(input$selected.pivot.project, {
    updateSelectInput(session = session, 
                      inputId = "selected.project.client", 
                      selected = pivot.client.project$CompanyName[pivot.client.project$ProjectName == input$selected.pivot.project])
  })
  
  # 1A - changing project to a brand new client
  observeEvent({req(input$selected.project.client == "New Client") 
    req(input$new.pivot.client)}, {
    
    old.client <- unique(pivot.client.project$CompanyName[pivot.client.project$ProjectName == input$select.project.admin])
    print(old.client)
    new.client <- input$new.pivot.client
    print(new.client)
    updateTextInput(session = session, 
                    inputId = "selected.project.name", 
                    value = sub(pattern = toupper(old.client), 
                                replacement = toupper(new.client), 
                                x = input$select.project.admin))
  })
  
  # 1B - changing project to a different current client
  observeEvent({req(input$selected.project.client != "New Client")}, {
      
      old.client <- unique(pivot.client.project$CompanyName[pivot.client.project$ProjectName == input$select.project.admin])
      new.client <- input$selected.project.client
      
      updateTextInput(session = session, 
                      inputId = "selected.project.name", 
                      value = sub(pattern = toupper(old.client), 
                                  replacement = toupper(new.client), 
                                  x = input$select.project.admin))
    })
  
  # save changes to project
  observeEvent(input$save.changes.project, {
    
    # add new employee to project
    observeEvent(input$project.not.team, {
      pivot.client.project <- add_row(as.data.frame(pivot.client.project),
                                      CompanyName = client.name,
                                      ProjectName = toupper(input$selected.project.name),
                                      EmployeeName = input$project.not.team,
                                      UserActive = 1)
      print("adding to team")
      print(pivot.client.project[pivot.client.project$ProjectName==input$selected.project.name,])
    })
    
    
    # update active status for those already on the team
    observeEvent(input$inactive.project.team, {
      pivot.client.project$UserActive[(pivot.client.project$EmployeeName %in% input$inactive.project.team) &
                             (pivot.client.project$ProjectName == input$select.project.admin)] <- 1
      print("updating active status")
      print(pivot.client.project[pivot.client.project$ProjectName==input$selected.project.name,])
    })
    
    # update client name
    client.name <- ifelse(test = input$selected.project.client == "New Client", 
                          yes = input$new.pivot.client,
                          no = input$selected.project.client)
    
    pivot.client.project$CompanyName[pivot.client.project$ProjectName == input$select.project.admin] <- client.name
    
    # update project name
    pivot.client.project$ProjectName[pivot.client.project$ProjectName == input$select.project.admin] <- input$selected.project.name
    
    # save updated rds
    print(input$select.project.admin)
    print(input$selected.project.name)
    write_rds(pivot.client.project, paste0(getwd(), "/data/pivot.client.project.rds"))
    
    
    showModal(modalDialog(
      title = "Well done!",
      paste0("You have made changes to the project ", toupper(input$selected.project.name)),
      footer = tagList(
        modalButton("Awesome!")
      )
    ))
    
  })
  
  
  ###### DELETE PROJECT
  observe({
    proj.delete.choices <- pivot.time %>% 
      group_by(ProjectName) %>%
      summarise(time.count=n()) %>%
      right_join(pivot.client.project, by = "ProjectName") %>%
      filter(is.na(time.count)) %>%
      distinct(ProjectName) %>%
      arrange(ProjectName) %>%
      pull(ProjectName)
    
    if(length(proj.delete.choices) == 1){
      proj.delete.choices <- list(proj.delete.choices)
    }
    
    if(length(proj.delete.choices) > 0){
      updateSelectInput(session = session, 
                        inputId = "delete.project", 
                        choices = list(`Select a Project` = '',
                                       `Projects` = proj.delete.choices))
    }
  })
  
  observeEvent(input$delete.project.button, {
    
    req(input$delete.project)
    
    pivot.client.project <- pivot.client.project[pivot.client.project$ProjectName != input$delete.project,]
    write_rds(pivot.client.project, paste0(getwd(), "/data/pivot.client.project.rds"))
    
    updateSelectInput(session = session,
                      inputId = "delete.project", 
                      selected = "Select a Project` = ''")
    
    showModal(modalDialog(
      title = "I hope you meant to do that!",
      paste0("You've deleted the project ", toupper(input$delete.project)),
      footer = tagList(
        modalButton("What's done is done!")
      )
    ))
  })

  
###### EMPLOYEE - PROJECT ADMIN
  ###### REMOVE EMPLOYEE(S) FROM A PROJECT
  # update Select Input to something of use
  observe({
    proj.employ.remove.choice <- pivot.time %>% 
      group_by(ProjectName, EmployeeName) %>% 
      summarise(time.count = n()) %>% 
      right_join(pivot.client.project, by = c("ProjectName", "EmployeeName")) %>% 
      filter(is.na(time.count)) %>% 
      distinct(ProjectName) %>% 
      arrange(ProjectName) %>% 
      pull(ProjectName)
    
    if(length(proj.employ.remove.choice) == 1){
      proj.employ.remove.choice <- list(proj.employ.remove.choice)
    }
    
    if(length(proj.employ.remove.choice) > 0){
      updateSelectInput(session = session, 
                        inputId = "remove.employee.project", 
                        choices = list(`Select a Project` = '', 
                                       `Projects` = proj.employ.remove.choice))
    }
   
  })
  
  observeEvent(input$remove.employee.button, {
    req(input$remove.employee.project)
    
    employee.no.time <- pivot.client.project %>% 
      filter(ProjectName == input$remove.employee.project) %>%
      filter(!(EmployeeName %in% pivot.time$EmployeeName[pivot.time$ProjectName == input$remove.employee.project])) %>% 
      filter(EmployeeName %in% pivot.members$EmployeeName[pivot.members$Current == T]) %>%
      distinct(EmployeeName) %>% 
      select(EmployeeName) %>% 
      pull(EmployeeName)
    
    showModal(remove_employee_project_Modal(session, 
                                            project = input$remove.employee.project, 
                                            employee = employee.no.time))
  })
  
  observeEvent(input$really.remove.employee, {
    pivot.client.project <- pivot.client.project[pivot.client.project$ProjectName != input$remove.employee.project &
                                                   pivot.client.project$EmployeeName != input$remove.employee,]
    write_rds(pivot.client.project, paste0(getwd(), "/data/pivot.client.project.rds"))

    updateSelectInput(session = session,
                      inputId = "remove.employee.project",
                      selected = "`Select a Project` = ''")

    showModal(modalDialog(
      title = "Well done!",
      paste0(input$remove.employee, " was removed from ", input$remove.employee.project),
      footer = tagList(
        modalButton("Awesome!")
      )
    ))
  })
  
  

###### EMPLOYEE ADMIN
  # display modal for new employee info
  observeEvent(input$employee.admin.button, {
    
    req(input$select.employee.admin)
    
    if(input$select.employee.admin == "New Employee") {
      showModal(new_employee_Modal(session))
    } else {
      showModal(edit_employee_Modal(session, employee = input$select.employee.admin))
    }
  })

###### NEW EMPLOYEE
  # new employee details
  observeEvent(input$employee.full.name, {
    
    employee.name <- unlist(strsplit(input$employee.full.name, split = " "))
    
    updateTextInput(session = session, inputId = "employee.preferred.name", 
                    value = employee.name[1])
    
    if(length(employee.name) > 1){
      updateTextInput(session = session, inputId = "employee.email", 
                      value = str_c(employee.name[1], ".", 
                                    employee.name[length(employee.name)],
                                    "@pivotsciences.com"))
    }
    
  })
  
  # save new employee details
  observeEvent(input$save.employee, {
    
    req(input$employee.full.name, input$employee.email)
    
    req(!(input$employee.email %in% pivot.members$EmailAddress))
    
    # add employee to pivot.memebrs
    pivot.members <- add_row(as.data.frame(pivot.members),
                             EmployeeID = as.integer(max(pivot.members$EmployeeID) + 1),
                             EmployeeName = input$employee.full.name,
                             EmailAddress = input$employee.email,
                             PreferredName = input$employee.preferred.name,
                             Current = TRUE,
                             Admin = input$admin)
    
    write_rds(pivot.members, paste0(getwd(), "/data/pivot.members.rds"))
    
    # add employee to selected projects
    project.info <- data_frame(project = input$employee.projects, 
                           company = NA)
    
    for (i in seq_along(project.info$project)){
      project.info$company[i] <- unique(as.vector(pivot.client.project$CompanyName[pivot.client.project$ProjectName == project.info$project[i]]))
      }
    
    pivot.client.project <- add_row(as.data.frame(pivot.client.project),
                                    CompanyName = project.info$company,
                                    ProjectName = project.info$project,
                                    EmployeeName = input$employee.full.name,
                                    UserActive = 1)

    write_rds(pivot.client.project, paste0(getwd(), "/data/pivot.client.project.rds"))
    
    updateSelectInput(session = session, inputId = "select.employee.admin", 
                      selected = "`Select an Employee` = ''")
    
    
    showModal(modalDialog(
      title = "Well done!",
      paste0(input$employee.full.name, " has been added to team Pivot Sciences."),
      footer = tagList(
        modalButton("Awesome!")
      )
    ))
    
  })

###### EDIT EMPLOYEE
  
  observeEvent(input$save.changes.employee, {
    
    req(input$employee.preferred.name, input$employee.email, input$employee.full.name)
    
    pivot.members$PreferredName[pivot.members$EmployeeName == input$select.employee.admin] <- input$employee.preferred.name
    pivot.members$EmailAddress[pivot.members$EmployeeName == input$select.employee.admin] <- input$employee.email
    pivot.members$Admin[pivot.members$EmployeeName == input$select.employee.admin] <- input$admin
    pivot.members$Current[pivot.members$EmployeeName == input$select.employee.admin] <- input$current
    pivot.members$EmployeeName[pivot.members$EmployeeName == input$select.employee.admin] <- input$employee.full.name
    
    write_rds(pivot.members, paste0(getwd(), "/data/pivot.members.rds"))
    
    print(pivot.members[pivot.members$EmployeeName == input$select.employee.admin,])
    
    updateSelectInput(session = session, inputId = "select.employee.admin", 
                      selected = "`Select an Employee` = ''")
    
    
    showModal(modalDialog(
      title = "Well done!",
      paste0("You have made changes to ", input$employee.preferred.name, "'s details"),
      footer = tagList(
        modalButton("Awesome!")
      )
    ))
      
  })
  
  ###### DELETE EMPLOYEE
  
  observe({
    employ.delete.choice <- pivot.time %>% 
      group_by(ProjectName, EmployeeName) %>% 
      summarise(time.count = n()) %>% 
      right_join(pivot.members, by = "EmployeeName") %>% 
      filter(is.na(time.count)) %>% 
      distinct(EmployeeName) %>% 
      arrange(EmployeeName) %>% 
      pull(EmployeeName)
    
    if(length(employ.delete.choice) == 1){
      employ.delete.choice <- list(employ.delete.choice)
    }
    
    if(length(employ.delete.choice) > 0){
    updateSelectInput(session = session, 
                      inputId = "delete.employee", 
                      choices = list(`Select an Employee` = '', 
                                     `Employees` = employ.delete.choice))
    }
    
  })
  
  
  observeEvent(input$delete.employee.button, {
    
    req(input$delete.employee)
    
    pivot.members <- pivot.members[pivot.members$EmployeeName != input$delete.employee,]
    write_rds(pivot.members, paste0(getwd(), "/data/pivot.members.rds"))

    updateSelectInput(session = session,
                      inputId = "delete.employee",
                      selected = "`Select an Employee` = ''")

    showModal(modalDialog(
      title = "I hope you meant to do that!",
      paste0("You've deleted ", toupper(input$delete.employee)),
      footer = tagList(
        modalButton("What's done is done!")
      )
    ))
  })

}