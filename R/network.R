#' Get the cached graph as a tidygraph
#'
#' Takes a moment. We recommend saving the resulting graph in a `.rds`
#' file for fast access later on.
#'
#' @param core_only Logical indicating whether or not to return
#'   just the core nodes. A node in the "core" of the cache if you
#'   explicitly requested it. Equivalently, if it has its own file
#'   in the cache. Other nodes will appear in the graph because of
#'   they are connected to nodes in the core. So we have complete
#'   information about the friendships of nodes in the core, and
#'   only very partial information about the remaining nodes.
#'   Defaults to `FALSE`.
#'
#' @return The network as a [tidygraph::tbl_graph()] object.
#'   Automatically removes information on protected users.
#' @export
#'
get_network_as_tidygraph <- function(core_only = FALSE) {

  node_data <- get_node_data()
  edge_data <- get_edge_data()

  graph <- edge_data %>%
    dplyr::distinct() %>%
    tidygraph::as_tbl_graph() %>%
    tidygraph::activate(nodes) %>%
    tidygraph::left_join(node_data, by = c("name" = "user_id")) %>%
    tidygraph::filter(!protected)

  if (core_only)
    graph <- tidygraph::filter(graph, !is.na(status_id))

  graph
}

get_network_dir <- function() {
  file.path(get_cache_dir(), "network")
}

get_edge_data <- function() {

  all_files <- list.files(get_network_dir(), full.names = TRUE)
  is_edge <- stringr::str_detect(all_files, "edge")

  edge_files <- all_files[is_edge]
  purrr::map_dfr(edge_files, readRDS)
}

get_node_data <- function() {

  all_files <- list.files(get_network_dir(), full.names = TRUE)
  is_node <- stringr::str_detect(all_files, "node")

  node_files <- all_files[is_node]
  purrr::map_dfr(node_files, readRDS)
}

