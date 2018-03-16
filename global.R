isDev <<- TRUE
if (!("pacman" %in% rownames(installed.packages()))) {
  install.packages("pacman")
}

if (!("shinyCleave" %in% rownames(installed.packages()))) {
  devtools::install_github("carlganz/shinyCleave")
}

install.packages(file.path(normalizePath("."),"theapp_0.1.0.tar.gz"), repos = NULL, type = "source")
library(pacman)
p_load(shiny)
p_load(shinydashboard)
p_load(tidyverse)
p_load(lubridate)
p_load(stringr)
p_load(shinyjs)
p_load(rhandsontable)
p_load(shinyWidgets)
p_load(shinyCleave)
p_load(DT)
p_load(assertthat)
p_load(theapp)
p_load(jsonlite)
p_load(rredis)
p_load(doParallel)
p_load(foreach)

config <- fromJSON("config.json")
if(grepl(pattern = "peterf",x = Sys.info()["user"],ignore.case = T)){
  rootfolder = config$rootfolderpeterf
}else{
  rootfolder = config$rootfolder
}



codefolder = rootfolder
walk(list.files(path = paste0(codefolder,"modules"), full.names = TRUE,pattern = "*.R"),source)
walk(list.files(path = paste0(codefolder,"functions"), full.names = TRUE,pattern = "*.R"),source)


cat("Connecting to redis DB... ")
# system(paste0("docker run --name ",config$rediscontainer$name," -d -p 127.0.0.1:6369:6379 --restart=unless-stopped redis:alpine"), intern = TRUE)
tryCatch(
  #connect to redis database
  {
    if(checkExistance(config$rediscontainer$name)){
      establishConnection(host = config$rediscontainer$ip, port = config$rediscontainer$port)
      cat("Success: Connected\n")
    }else{
      stop()
    }
  }
  , error = function(e){cat("Error occured connecting to Redis\n");
                  print(e); }
)

tryCatch(
  #check redis database
  {
    if(isEmpty()){
      cat("db empty, attempting to populate...\n")
      host_ = config$rediscontainer$ip
      port_ = config$rediscontainer$port 

      time.data = readRDS(file = file.path(normalizePath("."), "data", "pivot.time.rds"))
      project.data = readRDS(file = file.path(normalizePath("."), "data", "pivot.client.project.rds"))
      user.data = readRDS(file = file.path(normalizePath("."), "data", "pivot.members.rds"))
      add_timecaputure_dfs <- function(host, port, time.data){
        # add time capture data (name:date:timecapture)
        usersInput <- time.data %>% select(EmployeeName, TimeCaptureDate) %>% distinct()
        usersInput$shortName <- unlist(purrr::map(strsplit(usersInput$EmployeeName, split = " "), createShortName))
        establishConnection(host, port)
        for (i in 1:nrow(usersInput)) {
          out <- usersInput[i,] %>% select(shortName, EmployeeName, TimeCaptureDate) %>% left_join(time.data, by = c("EmployeeName", "TimeCaptureDate"))
          # key_ <- create_timecapture_key(shortName = out$shortName[1], TimeCaptureDate = TimeCaptureDate[1])
          key_ <- paste(out$shortName[1], as.character(out$TimeCaptureDate[1]), "timecapture", sep = ":")
          redisSet(key = key_, value = out)
        }
      }
      add_timecaputure_dfs(host = host_, port = port_, time.data)
      add_user_projects_dfs <- function(host, port, project.data){
        #clean and add user projects
        #add user projects (name:projects)
        project.data$shortName <- unlist(purrr::map(strsplit(project.data$EmployeeName, split = " "), createShortName))
        users <- project.data %>% ungroup() %>% select(shortName) %>% unique() %>% as.data.frame()
        for (i in 1:nrow(users)) {
          out <- project.data %>% filter(users$shortName[i] == shortName)
          key_ <- paste(users$shortName[i], "project", sep = ":")
          redisSet(key = key_, value = out)
        }
      }
      add_user_projects_dfs(host = host_, port = port_, project.data)
      add_user_df <- function(host, port, user.data){
        #clean and add user info
        user.data$shortName <- unlist(purrr::map(strsplit(user.data$EmployeeName, split = " "), createShortName))
        redisSet(key = "user", value = user.data)
      }
      add_user_df(host = host_, port = port_, user.data)
      cat("Success: db populated\n")
    }else if(!isEmpty()){
      stop()
    }
  }
  , error = function(e){cat("Redis is not empty\n")}
)



######################

user.tasks <- c("administration", "conference", "consulting", "development",
                "human resources", "meeting", "presentation",
                "research & learning", "sales & marketing", "support",
                "training", "travel")
userName <- NULL

# leave.type = c("sick leave", "annual leave", "public holiday",
#                "study leave", "family responsibility leave", "unpaid leave")
# 

