safe_get_friends <- function(node, ...) {
  friends <- get_friends(node, ..., verbose = FALSE)

  if (nrow(friends) == 0)
    return(empty_edgelist())

  colnames(friends) <- c("from", "to")
  friends
}

safe_get_followers <- function(node, ...) {
  followers <- get_followers(node, ..., verbose = FALSE)

  if (all(is.na(followers$user_id)))
    return(empty_edgelist())

  colnames(followers) <- "from"
  followers$to <- node

  followers
}

#' Visit a node in the twitter graph and add it to the cache
#'
#' @param node The user id of a Twitter user. *Not* the screen name.
#' @param force Logical indicating if the data for `node` should be
#'   updated even if `node` is already in the cache. Defaults to `FALSE`.
#'
#' @return Nothing, called for side effects.
#' @export
#'
visit <- function(node, force = FALSE, token = NULL) {

  if (length(node) != 1)
    stop("`node` must be a vector with a *single* user id.", call. = FALSE)

  if (!force && in_cache(node))
    invisible()

  # sampling strategy: assume that if you aren't in the first 5000
  # friends of followes, you'll get picked up from the other side
  # of the edge since most of the accounts i care about have small
  # overall friend and follower counts, so it's unlikely you get
  # missed on both ends. if you do, whatever, an edge goes missing.

  logger::log_debug("Attempt to get friends and followers for {node}.")

  friends <- safe_get_friends(node, retryonratelimit = TRUE, token = token)
  followers <- safe_get_followers(node, retryonratelimit = TRUE, token = token)

  logger::log_debug("Successful sampled friends and followers for {node}.")

  # note: some of these edges may be redundant
  # an edge can be sampling from both the follower
  # and following side
  edge_data <- dplyr::bind_rows(friends, followers)

  logger::log_debug("Looking up user data for {node}")

  node_data <- lookup_users(node, token = token)

  logger::log_debug("Writing data to cache for {node}")

  cache_edge_data(node, edge_data)
  cache_node_data(node, node_data)

  logger::log_debug("Data successfully written to cache for {node}")

  invisible()
}

#' @export
sample_twitter_graph <- function() {

  create_cache_if_needed()
  refresh_requests()

  num_tokens <- get_number_of_tokens()
  requests <- get_current_requests()

  if (length(requests) < 1) {
    message("No requests are currently in the queue.")
    invisible()
  }

  tokens <- list()

  for (i in 1:num_tokens)
    tokens[[i]] <- get_registered_token(i)

  eta <- Sys.time() + length(requests) * 60 / num_tokens

  message(
    glue(
      "\n", "Visiting {length(requests)} nodes with {num_tokens} tokens",
      "\n", "ETA (sampling 1 node/min/token): {eta}", "\n"
    )
  )

  while (length(requests) > 0) {

    logger::log_info("Remaining requests: {length(requests)}")

    for (i in 1:num_tokens) {

      if (length(requests) < i)
        break

      logger::log_info("Visiting {requests[i]} using token {i}.")

      tryCatch(
        visit(requests[i], token = tokens[[i]]),

        error = function(cnd) {
          logger::log_info("Failed to visit {requests[[i]]}.")
          add_failure(requests[[i]])
        }
      )
    }

    logger::log_info("Refreshing requests and sleeping for a minute.")

    refresh_requests()
    requests <- get_current_requests()

    if (length(requests) == 0)
      break

    Sys.sleep(60)
  }

  num_failures <- length(get_current_failures())

  if (num_failures > 0)
    warning(
      glue(
        "Failed to sample {num_failures} nodes. ",
        "Use `get_current_failures()` to see these nodes."
      ),
      call. = FALSE
    )

  logger::log_info("Done sampling.")
  refresh_requests()
}
