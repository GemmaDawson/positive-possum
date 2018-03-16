#' Function to get userDF
#'
#' This function takes the user name (email) and returns the associated data.frame
#'
#' @param names_ the user name (email)
#'
#' @return this returns a data.frame that contains user information
#'
#' @export
getUserDF <- function(conf, userName){
  userDF <- redisGetThis(conf, key_ = "user")
  return(userDF[userDF$EmailAddress == userName,])
}
