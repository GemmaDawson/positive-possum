#' Function to transform HH:MM to Decimal Hours
#'
#' This function transforms string of HH:MM to decimal hours
#'
#' @param hhmm string of format HH:MM which is converted to decimal hours 
#'
#' @return this function returns either single double or vector of doubles
#'
#' @export
hhmm_to_duration <- function(hhmm){
  # the function to potentially vectorise
  convertToDecimalHours <- function(HHMM){
    hh <- as.numeric(unlist(strsplit(HHMM, ":"))[1])
    mm <- round(as.numeric(unlist(strsplit(HHMM, ":"))[2])/60, digits = 2)
    duration = hh + mm
    return(duration)
  }
  if(length(hhmm) < 2){
    return(convertToDecimalHours(hhmm))
  } else if(length(hhmm) > 1){
    return(as.numeric(vapply(X = hhmm, FUN = convertToDecimalHours, FUN.VALUE = c(0))))
  }
}

