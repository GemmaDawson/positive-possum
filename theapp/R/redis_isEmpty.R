#' Function to determine whether redis db is empty
#'
#' This function takes no input and returns a boolean
#'
#' @return this function returns a boolean
#'
#' @export
isEmpty <- function(){
  tryCatch(
    #check if db is empty
    {
      dbSize = redisDBSize()
      if(!is.na(as.integer(dbSize))){
        if(as.integer(dbSize) < 1){
          return(TRUE)
        }else{
          return(FALSE)
        }
      }else{
        stop()
      }
    }
    , error = function(e){paste0(e)}
  )
}
