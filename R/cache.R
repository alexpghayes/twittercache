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

#' See how many users are in the cache
#'
#' @return An integer.
#'
#' @export
#'
get_cache_size <- function() {

  create_cache_if_needed()
  network_dir <- get_network_dir()

  # two files for each user in the cache
  length(list.files(network_dir, full.names = TRUE)) / 2
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

#' Update a version 0.1.0 cache to a version 0.2.0 cache
#'
#' Slow. Sets `sampled_at` column in node data based on the
#' most recent time the node data was modified. This
#' should be the time of sampling unless you've been
#' manually moving files around or something.
#'
#' If this crashes halfway through (which should only
#' happen due to user interrupt, fingers crossed)
#' you *should* be able to just run it again.
#'
#' You only need to update your cache if you sampled
#' users using `twittercache` version 0.1.0.
#'
#' @export
#'
update_cache <- function() {

  all_files <- list.files(get_network_dir(), full.names = TRUE)
  is_node <- stringr::str_detect(all_files, "node")

  node_files <- all_files[is_node]
  edge_files <- all_files[!is_node]

  for (file in node_files) {
    logger::log_info("Updating {file}")
    update_node_file(file)
  }

  for (file in edge_files) {
    logger::log_info("Updating {file}")
    update_edge_file(file)
  }

  message("Successfully updated cache!")
}

update_edge_file <- function(edge_path) {

  edge_data <- readr::read_rds(edge_path)

  if (nrow(edge_data) < 1) {
    file.remove(edge_path)
  }

 edge_data %>%
    dplyr::mutate_all(as.integer64) %>%
    readr::write_rds(edge_path)
}

update_node_file <- function(node_path) {
  node_data <- readr::read_rds(node_path)

  if (nrow(node_data) < 1) {
    file.remove(node_path)
  }

  node_data <- node_data %>%
    dplyr::mutate_at(dplyr::vars(user_id), as.integer64)

  if (!("sampled_at" %in% colnames(node_data)))
    node_data <- tibble::add_column(
      node_data,
      sampled_at = file.info(node_path)$ctime,
      .before = TRUE
    )

  readr::write_rds(node_data, node_path)
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
  readr::write_rds(edge_data, get_edge_path(node))
}

cache_node_data <- function(node, node_data) {
  readr::write_rds(node_data, get_node_path(node))
}
