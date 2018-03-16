# docker functions
# checkExistance(container)  returns TRUE/FALSE
# getIP(container)  returns String: IP/Error message
# peterf: for testing
# containerName = config$rediscontainer$name
checkExistance <- function(container){
  tryCatch(
    {
      if(nchar(container) < 2){
        stop()
      } else {
        status = system(paste0("docker ps -q -f name=", container), intern = TRUE)
      }
      if(status > 0){
        return(TRUE)
      } else {
        stop()
      }
    }
    , error = function(e) return(FALSE)
  )
}
# checkExistance("some-redis")
# checkExistance("")
# checkExistance("abcd")
getIP <- function(container){
  tryCatch(
    {
      if(checkExistance(container)){
        ip = system(paste0("docker inspect --format='{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' ", container), intern = TRUE)
        return(ip)
      } else {
        stop()
      }
    }
    # TODO need to implement proper error messages
    ,error = function(e) return(paste0(e))
  )
}
# getIP("")
# getIP("some-redis")
# TODO
# getStatus <- function(container){
#   tryCatch(
#     {
#       if(checkExistance(container)){
#         ip = system(paste0("docker ps --filter name=", container," --filter status=running"), intern = TRUE)
#         return(ip)
#       } else {
#         stop()
#       }
#     }
#     ,error = function(e) return("Something went wrong?")
#   )
# }
# getStatus("")
# getStatus("some-redis")
# TODO does not works as expected, need to have a look at the return type error
# dockerInit <- function(container){
#   tryCatch(
#     {
#       if(!checkExistance(container)){
#         system(paste0("docker run --name ", container," -d redis:4.0.6-alpine -p 6379:6279"), intern = TRUE)
#       } else {
#         stop()
#       }
#     }
#     # TODO need to implement proper error messages
#     ,error = function(e) return(logger(message = "Could not initialise redis container", e))
#   )
# }
