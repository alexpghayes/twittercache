empty_edgelist <- function() {
  tibble::tibble(from = character(0), to = character(0))
}

utils::globalVariables(c("query", "protected", "status_id", "nodes"))
