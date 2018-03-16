personal_details_module_UI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    fluidRow(box(title = "PERSONAL DETAILS", status = "primary", solidHeader = F, 
                 width = NULL,
                 renderText(inputId = ns("full.name")),
                 renderText(inputId = ns("preferred.name")),
                 actionButton(inputId = ns("edit.personal.details"), label = "EDIT", width = "100px")
                 )
             )
    )
}

personal_details_module <- function(input, output, session){
  output$full.name <- textOutput(str_c("Full Name: ", pivot.members$EmployeeName[pivot.members$EmployeeName == user]))
  
  output$preferred.name <- textOutput(str_c("Preferred Name: ", pivot.members$PreferredName[pivot.members$EmployeeName == user]))
  
}