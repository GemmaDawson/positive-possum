#' Function to create a column of delete buttons
#'
#' This function creates a column of delete buttons
#'
#' @param FUN
#' @param len
#' @param id
#'
#' @return this function returns nothing
#'
#' @export
delete_buttons <- function(FUN, len, id, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, i), ...))
  }
  inputs
}
