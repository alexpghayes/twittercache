#' Robustly sample from the twitter graph
#'
#' Once you have added users to the request queue, run
#' `sample_twitter_graph()`. That's it. Data about each
#' user is saved into the cache as soon it is received.
#'
#' If your R session crashes, you lose internet
#' connection, whatever, it doesn't matter. Just run
#' `sample_twitter_graph()` again and the sampling will pick
#' up right where it left off.
#'
#' Sampling will automatically use all registered tokens,
#' and will respect rates limits. When we reach an API
#' rate limit, we simply wait for the shortest amount of
#' time until the rate limit resets for the next
#' available token.
#'
#' @export
#' @seealso [register_token()], [request()]
#'
sample_twitter_graph <- function() {

  create_cache_if_needed()
  refresh_requests()

  num_tokens <- get_number_of_tokens()
  requests <- get_request_ids()

  if (length(requests) < 1) {
    message("No requests are currently in the queue.")
    invisible()
  }

  tokens <- get_all_tokens()

  eta <- Sys.time() + length(requests) * 60 / num_tokens

  message(
    glue(
      "\n", "Visiting {length(requests)} nodes with {num_tokens} tokens",
      "\n", "ETA (sampling 1 node/min/token): {eta}", "\n"
    )
  )

  while (length(requests) > 0) {

    node <- requests[1]

    logger::log_info("Visiting {node}.")

    tryCatch(
      visit(node),

      error = function(cnd) {
        logger::log_info("Failed to visit {node}.")
        add_failure(node)
      }
    )

    refresh_requests()
    requests <- get_request_ids()

    logger::log_info("Remaining requests: {length(requests)}")
  }

  num_failures <- length(get_failure_ids())

  if (num_failures > 0)
    warning(
      glue(
        "Failed to sample {num_failures} nodes. ",
        "Use `get_failures()` to see these nodes."
      ),
      call. = FALSE
    )

  logger::log_info("Done sampling.")
  refresh_requests()
}

#' Visit a node in the twitter graph and add it to the cache
#'
#' The logic here is fundamentally oriented around visiting
#' a single user at a time, and adding them to the cache as
#' soon as we have their data.
#'
#' Note that since the logic here is fundamentally about
#' sampling every user, if we visit an entire neighborhood
#' we'll pick up most edges twitter, once from each accompanying
#' node.
#'
#' We only take the first 5,000 friends and 5,000 followers
#' for the time being. The idea is that for important edges,
#' the edge will get picked up from the other node. This may
#' miss out on edges between node both with huge follower
#' and following counts, but who cares about those.
#'
#' @param node The user id of a Twitter user. *Not* the screen name.
#'
#' @keywords internal
#'
visit <- function(node) {

  if (length(node) != 1)
    stop("`node` must be a vector with a *single* user id.", call. = FALSE)

  logger::log_debug("Attempt to get friends and followers for {node}.")

  friends <- safe_get_friends(node)
  followers <- safe_get_followers(node)

  logger::log_debug("Successful sampled friends and followers for {node}.")

  edge_data <- dplyr::bind_rows(friends, followers)

  logger::log_debug("Looking up user data for {node}")

  node_data <- safe_lookup_user(node)

  logger::log_debug("Writing data to cache for {node}")

  cache_edge_data(node, edge_data)
  cache_node_data(node, node_data)

  logger::log_debug("Data successfully written to cache for {node}")

  invisible()
}
