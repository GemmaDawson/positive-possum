#' Function to create short names
#'
#' This function takes string that conatins the user's full name and splits it up the contruct an unique name for database indexing
#'
#' @param names_ the user's full name
#'
#' @return this returns a short version of the name for the database
#'
#' @export
createShortName <- function(names_){
  tryCatch(
    {
      if(is.character(names_) || is.list(names_)){
        if(is.character(names_)){
          list_index = names_
        } else if(is.list(names_)){
          list_index = names_[[1]]
        }
        length_of_names_ = length(list_index)
        firstName = tolower(list_index[1])
        lastName = tolower(list_index[length_of_names_])
        final = paste(firstName, substr(lastName, 0, 1), "001", sep = "", collapse = "")
        return(final)
      } else {
        stop()
      }
    }, error = function(e){paste0(e)}
  )
}
