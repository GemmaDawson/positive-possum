get_user_projects <- function(conf, userDF){
  redisGetThis(conf, paste(userDF$shortName, "project", sep = ":"))
}
