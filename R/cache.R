#' Check if a user is in the cache
#'
#' @param users A vector screen names or user IDs of Twitter users.
#'
#' @return A logical vector.
#'
#' @export
#' @family cache management
#'
user_in_cache <- function(users) {

  create_cache_if_needed()

  nodes <- rtweet::lookup_users(users)$user_id
  in_cache(nodes)
}

#' Remove all sampled users from the cache
#'
#' Will ask you for confirmation before proceeding because
#' of the how time consuming it is to sample users.
#'
#' @export
#' @family cache management
#'
clear_cache <- function() {

  create_cache_if_needed()

  if (usethis::ui_yeah("I want to remove all Twitter user data from my cache")) {

    network_dir <- get_network_dir()
    user_files <- list.files(network_dir, full.names = TRUE)

    for (file in user_files)
      file.remove(file)
  }
}

#' Remove suspended and deleted accounts from the cache
#'
#' Not yet implemented.
#'
#' @export
clean_cache <- function() {
  create_cache_if_needed()
  .NotYetImplemented()
}

#' Check if nodes are in the cache
#'
#' @param nodes User ids of Twitter users. *Not* the screen name.
#'
#' @return A logical vector.
#' @keywords internal
#'
in_cache <- function(nodes) {

  have_node <- logical(length(nodes))
  cache_dir <- get_cache_dir()

  for (i in seq_along(nodes)) {

    node <- nodes[i]

    edge_path <- get_edge_path(node)
    node_path <- get_node_path(node)

    have_node[i] <- file.exists(edge_path) && file.exists(node_path)
  }

  have_node
}

create_cache_if_needed <- function() {
  cache_dir <- get_cache_dir()

  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir)
    dir.create(get_network_dir())
    dir.create(get_token_dir())
  }

}

get_cache_dir <- function() {
  path.expand("~/.twittergraph")
}

get_node_path <- function(node) {
  file.path(get_network_dir(), paste0(node, "_node.rds"))
}

get_edge_path <- function(node) {
  file.path(get_network_dir(), paste0(node, "_edge.rds"))
}

cache_edge_data <- function(node, edge_data) {
  saveRDS(edge_data, get_edge_path(node))
}

cache_node_data <- function(node, node_data) {
  saveRDS(node_data, get_node_path(node))
}
