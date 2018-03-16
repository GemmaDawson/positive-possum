get_all_projects <- function(conf_){
  return(do.call(rbind, lapply(redisKeys("*:project"), redisGetThis, conf = conf_)))
}
