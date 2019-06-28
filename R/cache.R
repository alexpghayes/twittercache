#' Check if a node is in the cache
#'
#' @param node The user id of a Twitter user. *Not* the screen name.
#'
#' @return Either `TRUE` or `FALSE`.
#'
in_cache <- function(node) {

  cache_dir <- get_cache_dir()

  edge_path <- file.path(cache_dir, "network", paste0(node, "_edge.rds"))
  node_path <- file.path(cache_dir, "network", paste0(node, "_node.rds"))

  file.exists(edge_path) && file.exists(node_path)
}

create_cache_if_needed <- function() {
  cache_dir <- get_cache_dir()

  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir)
    dir.create(file.path(cache_dir, "network"))
    dir.create(file.path(cache_dir, "tokens"))
  }

}

get_cache_dir <- function() {
  path.expand("~/.twittergraph")
}

cache_edge_data <- function(node, edge_data) {
  create_cache_if_needed()
  cache_dir <- get_cache_dir()
  edge_path <- file.path(cache_dir, "network", paste0(node, "_edge.rds"))
  saveRDS(edge_data, edge_path)
}

cache_node_data <- function(node, node_data) {
  create_cache_if_needed()
  cache_dir <- get_cache_dir()
  node_path <- file.path(cache_dir, "network", paste0(node, "_node.rds"))
  saveRDS(node_data, node_path)
}
