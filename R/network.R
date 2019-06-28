# TODO: see how protected accounts show up.
# i think we just have reduced information on them -- no edges
# when we sample the protected account itself, and minimal user
# data

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

#' Get the cached graph as tidygraph
#'
#' Takes a moment. We recommend saving the resulting graph in a `.rds`
#' file for fast access later on.
#'
#' @param neighbors_only Logical indicating whether nodes TODO
#'
#' @return The network as a [tbl_graph()] object. Removes protected nodes.
#' @export
#'
get_network_as_tidygraph <- function(core_only = FALSE) {

  node_data <- get_node_data()
  edge_data <- get_edge_data()

  graph <- edge_data %>%
    distinct() %>%
    as_tbl_graph() %>%
    activate(nodes) %>%
    left_join(node_data, by = c("name" = "user_id")) %>%
    filter(!protected)

  if (core_only)
    graph <- filter(graph, !is.na(status_id))

  graph
}
