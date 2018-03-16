#' Prints duration of current meeting
#'
#' This function takes in the duration of the meeting and prints the time pretty.
#'
#' @param runTime A rounded difftime of system time
#'
#' @return This function returns the pretty duration
#'
#' @examples
#' timeDisplay(round(difftime(Sys.time(), Sys.time() - 1, units='sec')))
#' timeDisplay(round(difftime(Sys.time(), Sys.time() - 1*60, units='sec')))
#' timeDisplay(round(difftime(Sys.time(), Sys.time() - 1*60*60, units='sec')))
#'
#' @export
timeDisplay <- function(runTime){
  tryCatch(
    {
      if(class(runTime) == 'difftime'){
        if(as.numeric(runTime) >= 3600){
          hr <- floor(as.numeric(runTime)/(60*60))
          min <- floor((as.numeric(runTime)/60)-floor(as.numeric(runTime)/(60*60))*60)
          sec <- as.numeric(runTime)%%60
          paste(
            hr
            , 'hr'
            , min
            , 'mins'
            , sec
            , 'secs'
          )
        } else if(as.numeric(runTime) >= 60){
          min <- floor(as.numeric(runTime)/60)
          sec <- as.numeric(runTime)%%60
          paste(
            min
            , 'mins'
            , sec
            , 'secs'
          )
        } else if(as.numeric(runTime) < 60){
          paste(
            round(as.numeric(runTime))
            , 'secs'
          )
        }
      }else{
        stop()
      }
    }
    , error = function(e){paste0(e)}
  )
}
