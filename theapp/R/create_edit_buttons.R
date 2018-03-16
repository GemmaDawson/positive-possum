#' Function to creates edit buttons
#'
#' This function creates a column of edit buttons
#'
#' @param FUN
#' @param len
#' @param id
#'
#' @return this function returns button
#'
#' @export
edit_buttons <- function(FUN, len, id, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, i,"_", runif(1)), ...))
  }
  inputs
}