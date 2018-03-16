# PROJECT INFO & EDIT
edit_project_Modal <- function(session, project, active.team, inactive.team) {
  
  ns <- session$ns
  
  modalDialog(
    
    title = project,
    
   fluidRow( 
    column(12, # project name
           textInput(inputId = ns("selected.project.name"), 
                     label = "Project Name", 
                     value = project))
    ),
   
   fluidRow(
    column(6,
           # client name
           selectInput(inputId = ns("selected.project.client"),
                       label ="Client",
                       choices = list(`Select a Client:` = '', 
                                      "New Client",
                                      `Current Clients` = sort(unique(pivot.client.project$CompanyName))), 
                       selected = pivot.client.project$CompanyName[pivot.client.project$ProjectName == project])),
   
    column(6, 
    # add new client if needed
    conditionalPanel(condition = sprintf("input['%s'] == 'New Client'", ns("selected.project.client")),
                     textInput(inputId = ns("new.pivot.client"), label = "New Client Name:"))
    )),
   
   hr(),
    
    # team - active = selected & not active = not selected
   fluidRow(
    column(4,
           checkboxGroupInput(inputId = ns("active.project.team"), label = "Active Team", 
                       choices = sort(pivot.members$EmployeeName[(pivot.members$EmployeeName %in% active.team) & 
                                                                   (pivot.members$Current == T)]), 
                       selected = pivot.members$EmployeeName[(pivot.members$EmployeeName %in% active.team) & 
                                                               (pivot.members$Current == T)]),
           hr(),
           checkboxGroupInput(inputId = ns("inactive.project.team"), label = "Non-active Team",
                              choices = sort(pivot.members$EmployeeName[(pivot.members$EmployeeName %in% inactive.team) &
                                                                          (pivot.members$Current == T)]))
           ),
    
    # current employees never assigned to project
    column(4,
           checkboxGroupInput(inputId = ns("project.not.team"), label = "Not assigned to project", 
                              choices = sort(pivot.members$EmployeeName[!(pivot.members$EmployeeName %in% active.team) &
                                                                          !(pivot.members$EmployeeName %in% inactive.team) &
                                                                          (pivot.members$Current == T)])
           )
    ),
    
    # former employees on the project
    column(4,
           checkboxGroupInput(inputId = ns("project.team"), label = "Former Employees", 
                              choices = sort(pivot.members$EmployeeName[(pivot.members$EmployeeName %in% inactive.team) & 
                                                                          (pivot.members$Current == F)]))
    )
    ),
    
    easyClose = F,
    
    footer = tagList(
      actionButton(ns("save.changes.project"), "SAVE", width = "75px"),
      # actionButton(ns("delete.project"), "DELETE", width = "75px"),
      modalButton("CANCEL")
    )
    
  )
}