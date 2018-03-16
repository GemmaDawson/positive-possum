#' Function to create single delete button
#'
#' This function creates a single delete button
#'
#' @param FUN
#' @param len
#' @param id
#'
#' @return this function returns nothing
#'
#' @export
delete_button <- function(FUN, len, id, ...) {
  inputs <- as.character(FUN(paste0(id, len), ...))
  inputs
}
