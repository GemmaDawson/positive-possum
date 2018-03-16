#' Wrapper for redis get
#'
#' This function takes as input the host ip the port and the query
#'
#' @param host the users short name
#' @param port the range of dates
#' @param inputQuery the range of dates
#'
#' @return this function returns the redis result
#'
#' @export
redisSetThis <- function(conf, key, value){
  # host = getIP(container = config$rediscontainer$name)
  host = conf$ip
  port = conf$port
  redisConnect(host, port, nodelay = FALSE)
  return(redisSet(key, value))
}
