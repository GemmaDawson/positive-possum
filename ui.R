header <-   dashboardHeader(title = img(src="IMG_0110.jpg",
                                        height = 50, align = "center", alt = "PIVOT - The App"),
                            titleWidth ="150px")
# header <-   dashboardHeader(title = img(src="http://pivotsciences.com/wp-content/uploads/2016/09/PivotSciencesLogo.png",
#                                         height = 50, align = "center", alt = "PIVOT - The App"),
#                             titleWidth ="150px")

dashboardPage(
  title = "Pivot - The App",

  skin = "black",
  header,
  dashboardSidebar(
    width = 150,
    sidebarMenu(
      menuItem("Home", tabName = "hometab", icon = icon("user-circle")),
      menuItem("Time Capture", tabName = "timecapturetab", icon = icon("clock-o")),
      menuItem("Meetings", tabName = "meetingtab", icon = icon("comments"))
      # if(pivot.members$Admin[pivot.members$EmployeeName == user] == T){
      #   menuItem("Database", tabName = "redisdbtab", icon = icon("database"))
      # }
      # menuItem("Research & Learning", tabName = "learningtab", icon = icon("graduation-cap")),
      # menuItem("Skills", tabName = "skillstab", icon = icon("compass")),
      # menuItem("Projects", tabName = "projecttab", icon = icon("folder-open")),
      # menuItem("Boost", tabName = "boosttab", icon = icon("dashboard"))
    )
  ),
  
  
  
  dashboardBody(
    tags$head(tags$style(HTML('/* logo */.skin-black .main-header .logo {background-color: #ffffff;}
                              /* logo when hovered */.skin-black .main-header .logo:hover {background-color: #fffffff;}
                              /* navbar (rest of the header) */.skin-black .main-header .navbar {background-color: #ffffff;}   
                              /* main sidebar */.skin-black .main-sidebar {background-color: #4e7680;}
                              /* active selected tab in the sidebarmenu */.skin-black .main-sidebar .sidebar .sidebar-menu .active a{background-color: #3d636a;}
                              /* other links in the sidebarmenu when hovered */.skin-black .main-sidebar .sidebar .sidebar-menu a:hover{background-color: #4e7680;}
                              /* box header */.box.box-solid.box-primary>.box-header {color:#fff; background:#4e7680} 
                              /* box border */.box.box-solid.box-primary{border-bottom-color:#4e7680; border-left-color:#4e7680; border-right-color:#4e7680; border-top-color:#4e7680;}
                              /* info box */.bg-aqua {background-color: #4e7680!important}
                     '))),
    includeCleave(),
    useShinyjs(),
    tabItems(
      # home page
      tabItem(tabName = "hometab",
              home_module_UI("home.page")),
      
      #time capture tab content
      tabItem(tabName = "timecapturetab",
              time_capture_module_UI("time.capture")),
      
      # meeting tab content
      tabItem(tabName = "meetingtab",
              meeting_tab_module_UI("meeting.data"))
  
    )
  )
)