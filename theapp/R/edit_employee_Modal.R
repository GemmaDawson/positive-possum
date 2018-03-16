# EMPLOYEE INFO & EDIT
edit_employee_Modal <- function(session, employee) {
  
  ns <- session$ns
  
  modalDialog(
    
    title = employee,
    
    textInput(inputId = ns("employee.full.name"), 
              label = "Full Name:", 
              value = employee),
    
    textInput(inputId = ns("employee.preferred.name"), 
              label = "Preferred Name:", 
              value = pivot.members$PreferredName[pivot.members$EmployeeName == employee]),
    
    textInput(inputId = ns("employee.email"), 
              label = "Email Address:",
              value = pivot.members$EmailAddress[pivot.members$EmployeeName == employee]),
    
    checkboxInput(inputId = ns("admin"), 
                  label = "Admin User?", 
                  value = pivot.members$Admin[pivot.members$EmployeeName == employee]),
    
    checkboxInput(inputId = ns("current"), 
                  label = "Current Employee?", 
                  value = pivot.members$Current[pivot.members$EmployeeName == employee]),
    
    easyClose = F,
    
    footer = tagList(
      actionButton(ns("save.changes.employee"), "SAVE", width = "75px"),
      modalButton("CANCEL")
    )
    
  )
}