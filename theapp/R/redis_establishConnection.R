#' Function to establish connection to redis db
#'
#' This function takes as input the host ip and the port so it can connect to redis
#'
#' @param host the host ip
#' @param port the port
#'
#' @return this function returns nothing
#'
#' @export
establishConnection <- function(host, port){
  redisConnect(host , port, nodelay = FALSE)
}
