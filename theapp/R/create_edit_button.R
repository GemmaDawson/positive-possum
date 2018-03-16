#' Function to create single edit button
#'
#' This function creates a single edit button
#'
#' @param host
#'
#' @return this function returns button
#'
#' @export
edit_button <- function(FUN, len, id, ...) {
  inputs <- as.character(FUN(paste0(id, len), ...))
  inputs
}