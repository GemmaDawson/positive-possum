# ADD NEW PROJECT AND ASSIGN TEAM TO PROJECT
new_project_Modal <- function(session) {
  
  ns <- session$ns
  
  modalDialog(
    
    title = "Add a New Project",
    
    selectInput(inputId = ns("pivot.client"), label = "Client:", 
                choices = list(`Select a Client:` = '', "New Client",
                               `Current Clients` = unique(pivot.client.project$CompanyName)), 
                selected = NA, multiple = F),
    
    conditionalPanel(condition = sprintf("input['%s'] == 'New Client'", ns("pivot.client")),
                     textInput(inputId = ns("new.pivot.client"), label = "New Client Name:",
                               width = "150px")),
    
    textInput(inputId = ns("project.name"), label = "Project Name:", 
              width = "250px", 
              placeholder = "PROJECT NAME"),
    
    checkboxGroupInput(inputId = ns("new.project.team"), label = "Team", 
                       choices = pivot.members$EmployeeName[pivot.members$Current == T], 
                       selected = NULL),
    
    easyClose = F,
    
    footer = tagList(
      actionButton(ns("save.project"), "ADD", width = "75px"),
      modalButton("CANCEL")
    )
    
  )
}
