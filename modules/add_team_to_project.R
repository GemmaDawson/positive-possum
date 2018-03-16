add_team_to_project_UI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    box(title = "ADD TEAM TO PROJECT", width = NULL, status = "primary", solidHeader = TRUE,
    selectInput(inputId = ns("selected.project"), label = "Select a project:",
                      choices = unique(pivot.client.project$ProjectName[pivot.client.project$UserActive == 1]),
                      selected = NULL, multiple = F, width = "400px"),
    
    actionButton(inputId = ns("add.to.project"), label = "ADD TO TEAM", width = "150px")
          ))
}

add_team_to_project <- function(input, output, session){
  
  addModal <- function(){
    
    ns <- session$ns
    
    modalDialog(
      
      title = str_c("Add team members to ", input$selected.project),

      uiOutput(ns("who.is.missing")),
      
      actionButton(ns("add.team"), "SAVE", width = "100px"),
      
      easyClose = T,

      footer = tagList(
        modalButton("CANCEL")
      )

    )
  }
  
  observeEvent(input$add.to.project, {
    showModal(addModal())
  })
  
  team <- reactive({ as.data.frame(pivot.client.project %>% 
                                     filter(ProjectName == input$selected.project) %>% 
                                     ungroup() %>% 
                                     select(EmployeeName))
  })

  notteam <- reactive({ as.data.frame(pivot.members %>% 
                                        filter(Current == T,
                                               !(EmployeeName %in% team()$EmployeeName)) %>% 
                                        ungroup() %>% 
                                        select(EmployeeName) %>% 
                                        arrange(EmployeeName))
  })

  output$who.is.missing <- renderUI({
    checkboxGroupInput(session$ns("add.to.team"), label = "Select team members to add:",
                choices = notteam()$EmployeeName, selected = NULL)
  })
  
  observeEvent(input$add.team, {
    pivot.client.project <- add_row(as.data.frame(pivot.client.project),
                                    CompanyName = pivot.client.project$CompanyName[pivot.client.project$ProjectName == input$selected.project][1],
                                    ProjectName = input$selected.project,
                                    EmployeeName = input$add.to.team,
                                    UserActive = 1)

    write_rds(pivot.client.project, "data/pivot.client.project.rds")
    
# SMALL THING TO FIX IN THE "SUCCESS" MESSAGE - LIST WHICH TEAM MEMBERS HAVE BEEN ADDED TO THE PROJECT
# ALSO, I WOULD LIKE THE SAVE BUTTON TO DISPLAY ONLY WHEN PEOPLE HAVE BEEN SELECTED OR THE SAVED MESSAGE 
# GIVES A PROPER DESCRIPTION OF WHAT HAS BEEN SAVED OR NOT.
    showModal(modalDialog(
      title = "Well done!",
      paste0("You've added people to ", input$selected.project),
      footer = tagList(
        "If this was done in error please email Gemma.Dawson@pivotsciences.com",
        modalButton("Awesome!")
      )
    ))
  })
}