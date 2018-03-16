#' The purpose of this function is to return user input 0000 as 00:00
#'
#' This function takes in the duration in 0000 and prints it as 00:00.
#'
#' @param inputId An iputId
#' @param label A label
#'
#' @return This function returns the duration.
#'
#' @export
timeInput <- function(inputId, label, value = "", width = NULL, placeholder = NULL, ...) {
  shiny::tagList(
    shinyCleave::myInput("numeric", inputId, label, value, width, placeholder, ...),
    shiny::tags$script(
      paste0('
      var cleave = new Cleave("#', inputId, '", {
        blocks: [2, 2],
        delimiter: ":",
      });
    ')
    )
  )
}
