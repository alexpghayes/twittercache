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
#'
#' @export
#' @family exporting
#'
get_network_as_tidygraph <- function(core_only = FALSE) {

  node_data <- get_node_data()
  edge_data <- get_edge_data()

  graph <- edge_data %>%
    tidygraph::as_tbl_graph() %>%
    tidygraph::activate(nodes) %>%
    tidygraph::left_join(node_data, by = c("name" = "user_id"))

  if (core_only)
    graph <- tidygraph::filter(graph, !is.na(status_id))

  graph
}


#' Export the edge data from the cache to an edge list
#'
#' As your cache grows in size, this gets slow pretty
#' quickly. I recommend saving the results to an `.rds`
#' file for convenience. I also recommend subsetting
#' as much as possible to the part of the graph you
#' are interested in. `get_edge_data()` is typically
#' much slower than [get_node_data()].
#'
#' If you only need the graph and don't need any data
#' on the nodes, coerce immediately to an `igraph`
#' object with `igraph::from_edgelist()`.
#'
#' @return A [tibble::tibble()] with two columns `to` and `from`.
#'   Entries in each column are Twitter user IDs and have
#'   type [bit64::integer64()]. This breaks from the `rtweet`
#'   tradition of storing user IDs as character vectors,
#'   because character encoding user IDs quickly becomes
#'   inefficient for even moderate sized caches, which can
#'   contain millions of edges.
#'
#'   To my knowledge, you should be able to use these columns
#'   just like you'd expect. If you are uncomfortable with
#'   [bit64::integer64()] columns, you can convert them to
#'   character columns with `as.character()`. **Do not**
#'   convert the columns to a base integer or numeric vector
#'   with `as.integer` or `as.numeric`: this will result in
#'   data loss.
#'
#' @export
#' @family exporting
#'
get_edge_data <- function() {

  all_files <- list.files(get_network_dir(), full.names = TRUE)
  is_edge <- stringr::str_detect(all_files, "edge")

  edge_files <- all_files[is_edge]
  edges <- purrr::map_dfr(edge_files, readr::read_rds)
  dplyr::distinct(edges)
}

#' Export the node data from the cache
#'
#' As your cache grows in size, this gets slow pretty
#' quickly. I recommend saving the results to an `.rds`
#' file for convenience. I also recommend subsetting
#' as much as possible to the part of the graph you
#' are interested in.
#'
#' @return A [tibble::tibble()] with one row for each user
#'   and many columns. The first column, `sampled_at`, records
#'   when we recorded data for a particular user. The rest of
#'   the columns are the output of [rtweet::lookup_users()],
#'   while I will not document in detail here. This is a
#'   mixture of information about the user profile, and
#'   also their most recent tweet (at the time of sampling).
#'
#'   **There is one key difference from `rtweet::lookup_users()`**
#'   output, which is the `user_id` column is a [bit64::integer64()]
#'   vector rather than a character vector. This is to facilitate
#'   easy joining with the edgelist produced by [get_edge_data()].
#'   It is okay to convert this column to a character vector if
#'   you would like, but **do not** convert the `user_id` column
#'   to a base integer or numeric vector with `as.integer` or
#'   `as.numeric`: this will result in data loss.
#'
#'   Another particularly useful column is the `screen_name`
#'   column, which contains the user handles that you use to
#'   interact with other users on Twitter.
#'
#' @details If you `request()`ed user A, and user A follows user B,
#'   but you did not request user B, user A will appear in the node
#'   data but user B will not. We call user A a *core* user here,
#'   and user B a *peripheral*. `twittercache` does not allow
#'   you to request information about protected users, but it is
#'   still possible that some peripheral users will be protected
#'   accounts.
#'
#' @export
#' @family exporting
#'
get_node_data <- function() {

  all_files <- list.files(get_network_dir(), full.names = TRUE)
  is_node <- stringr::str_detect(all_files, "node")

  node_files <- all_files[is_node]
  purrr::map_dfr(node_files, readr::read_rds)
}

get_network_dir <- function() {
  file.path(get_cache_dir(), "network")
}


