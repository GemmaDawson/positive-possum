# ADD NEW EMPLOYEE TO PIVOT TEAM
new_employee_Modal <- function(session) {
  
  ns <- session$ns
  
  modalDialog(
    
    title = "Add a New Employee",
    
    textInput(inputId = ns("employee.full.name"), label = "Full Name:"),
    
    textInput(inputId = ns("employee.preferred.name"), label = "Preferred Name:"),
    
    checkboxInput(inputId = ns("admin"), label = "Admin User?", value = F),
    
    textInput(inputId = ns("employee.email"), label = "Email Address:"),
    
    selectInput(inputId = ns("employee.projects"), label = "Projects", 
                choices = sort(unique(pivot.client.project$ProjectName)), 
                selected = c("PIVOT SCIENCES - HEAD OFFICE",
                             "PIVOT SCIENCES - RESEARCH"),
                multiple = T),
    
    easyClose = F,
    
    footer = tagList(
      actionButton(ns("save.employee"), "ADD", width = "75px"),
      modalButton("CANCEL")
    )
    
  )
}