#' Check if a node is in the cache
#'
#' @param node The user id of a Twitter user. *Not* the screen name.
#'
#' @return Either `TRUE` or `FALSE`.
#'
in_cache <- function(node) {

  # TODO: vectorize this

  cache_dir <- get_cache_dir()

  edge_path <- file.path(cache_dir, "network", paste0(node, "_edge.rds"))
  node_path <- file.path(cache_dir, "network", paste0(node, "_node.rds"))

  file.exists(edge_path) && file.exists(node_path)
}

#' Check if a user is in the cache
#'
#' TODO: Not vectorized (yet)
#'
#' @param node Either the screen name of user ID of a Twitter user.
#'
#' @return Either `TRUE` or `FALSE`.
#' @export
#'
user_in_cache <- function(user) {
  node <- lookup_users(user)$user_id
  in_cache(node)
}

#' Check if a user's entire neighborhood is in the cache
#'
#' @param node Either the screen name of user ID of a Twitter user.
#'
#' @return Either `TRUE` or `FALSE`.
#' @export
#'
neighborhood_in_cache <- function(user) {
  .NotYetImplemented()
}

#' Get the user IDs of all nodes in a user's neighborhood
#'
#' @param node Either the screen name of user ID of a Twitter user.
#'
#' @return A character vector of user IDs. May be empty if the user
#'   doesn't follow anyone.
#'
#' @export
#'
get_neighborhood <- function(user) {
  .NotYetImplemented()
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
  cache_dir <- get_cache_dir()
  edge_path <- file.path(cache_dir, "network", paste0(node, "_edge.rds"))
  saveRDS(edge_data, edge_path)
}

cache_node_data <- function(node, node_data) {
  cache_dir <- get_cache_dir()
  node_path <- file.path(cache_dir, "network", paste0(node, "_node.rds"))
  saveRDS(node_data, node_path)
}
