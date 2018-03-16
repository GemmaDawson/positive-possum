#' Function to initialise redis db
#'
#' This function takes as input the host ip and the port so it can connect to redis
#'
#' @param host the host ip
#' @param port the port
#' @param time.data a dataframe that contains all the time captured from timelive, these data.frames should be found with redisKeys("*:timecapture")
#' @param project.data a dataframe that contains all the projects from timelive, this data.frame can be found using redisKeys("*:project")
#' @param user.data a dataframe that contains all the user info from timelive, this data.frame can be found using redisKeys("user")
#'
#' @return this function returns nothing
#'
#' @export
firstRun <- function(host, port, time.data, project.data, user.data){
  #######################################################
  # # for testing
  # library(pacman)
  # p_load(tidyverse)
  # p_load(rredis)
  # p_load(theapp) # pload inception?
  # host <- "192.168.0.218"
  # port <- 6279
  # time.data <- read_rds(path = file.path(normalizePath(".."), "data", "pivot.time.rds"))
  # project.data <- read_rds(path = file.path(normalizePath(".."), "data", "pivot.client.project.rds"))
  # user.data <- read_rds(path = file.path(normalizePath(".."), "data", "pivot.members.rds"))
  #######################################################
  # timecapture (pivot.time) is a dataframe with the timecapture data in it.
  #               stores name:date:timecapture
  # __users__ (pivot.members) is a dataframe with the users data in it.
  #               stores users
  # projects (pivot.client.project) is a dataframe with the projects data in it.
  #               stores name:project
  #######################################################
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
  add_timecaputure_dfs(host, port, time.data)
  ##########################################################
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
  add_user_projects_dfs(host, port, project.data)
  ##########################################################
  add_user_df <- function(host, port, user.data){
    #clean and add user info
    user.data$shortName <- unlist(purrr::map(strsplit(user.data$EmployeeName, split = " "), createShortName))
    redisSet(key = "user", value = user.data)
  }
  add_user_df(host, port, user.data)
}
