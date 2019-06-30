#' Get screen names from the request queue
#'
#' **NOTE**: calls `rtweet::lookup_users()` without checking API
#'   rate limits. Unless you have an enormous (90,000+) number of
#'   requests, or have been using your tokens outside of `twittercache`
#'   this is extremely unlikely to cause a problem.
#'
#' @return A character vector of screen names.
#' @export
#'
#' @family managing requests
#'
get_requests <- function() {
  ids <- get_request_ids()

  if (length(ids) < 1)
    return(character(0))

  rtweet::lookup_users(ids, token = get_registered_token(1))$screen_name
}

#' Empty the request queue
#'
#' @export
#' @family managing requests
#'
clear_requests <- function() {
  saveRDS(character(0), request_path())
}

#' Add users to the request queue
#'
#' Note that this only adds users to the request queue, but
#' does not do any sampling of the Twitter graph itself. To
#' sample the Twitter graph, run [sample_twitter_graph()].
#'
#' @param screen_names A characters vector of screen names or user ids
#'   to the to the request queue.
#'
#' @param neighborhood Logical indicating whether the neighbors (both
#'   friends and followers) of the users in `screen_names` should be
#'   added the request queue as well. Defaults to `FALSE`.
#'
#' @export
#' @family managing requests
#' @seealso [sample_twitter_graph()]
#'
request <- function(screen_names, neighborhood = FALSE) {

  create_cache_if_needed()
  token <- get_registered_token(1)

  found <- rtweet::lookup_users(screen_names, token = token)
  not_found <- setdiff(screen_names, found$screen_name)

  if (length(not_found) > 0)
    for (missing in not_found)
      warning(glue("Could not find user: {missing}", sep = "\n"), call. = FALSE)

  new_nodes <- found$user_id
  add_request(new_nodes)

  if (neighborhood) {
    for (node in new_nodes) {
      followers <- safe_get_followers(node)
      friends <- safe_get_friends(node)

      add_request(friends$to)
      add_request(followers$from)
    }
  }
}

#' Add users to the request queue
#'
#' Also refreshes the request queue.
#'
#' @param user_ids One or most user IDS (*not* screen names) to add
#'   to the failure log.
#'
#' @keywords internal
#'
add_request <- function(user_ids) {

  current_requests <- get_request_ids()
  all_requests <- c(current_requests, user_ids)

  saveRDS(all_requests, request_path())
  refresh_requests()  # remove reduplicates and completes
}

#' Get user IDs from the request queue
#'
#' @return Character vector of users IDs that we need
#'   to sample.
#'
#' @keywords internal
#'
get_request_ids <- function() {

  if (!file.exists(request_path()))
    return(character(0))

  readRDS(request_path())
}

#' Get the path to the request queue
#'
#' The request queue is a character vector of user IDs (never
#' screen names) that gets stored as an `.rds` file.
#'
#' @return Path to `.rds` file.
#'
#' @keywords internal
#'
request_path <- function() {
  cache_dir <- get_cache_dir()
  file.path(cache_dir, "requests.rds")
}

#' Refresh the request queue
#'
#' Removes duplicates, users that we have failed to sample,
#' and users already in the cache.
#'
#' @details A fundamental assumption is that a node cannot be
#' in both the request queue and the failure log at the same time!
#'
#' @keywords internal
#'
refresh_requests <- function() {

  requests <- get_request_ids()
  failures <- get_failure_ids()

  requests <- setdiff(requests, failures)
  already_in_cache <- vapply(requests, in_cache, logical(1L))

  incomplete_requests <- unique(requests[!already_in_cache])
  saveRDS(incomplete_requests, request_path())
}

