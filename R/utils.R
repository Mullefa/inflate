all_distinct <- function(x) {
  dplyr::n_distinct(x) == length(x)
}


error <- function(...) {
  stop(..., call. = FALSE)
}


replace_numeric_nas <- function(data) {
  data[ ] <- lapply(data, function(x) {
    if (is.numeric(x)) {
      x[is.na(x)] <- 0
    }
    x
  })
  data
}


`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}


bracket <- function(x) {
  paste0("(", x, ")")
}
