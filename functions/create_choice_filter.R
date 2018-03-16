create_choice_filter <- function(col) {
  reactive({
    filter_values <- lapply(filters[-col], do.call, args = list())
    Reduce(`&`, filter_values, TRUE)
  })
}