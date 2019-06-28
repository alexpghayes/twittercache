#' @export
clear_requests <- function() {
  saveRDS(character(0), request_path())
}

#' @export
remove_requests <- function(user_ids) {
  .NotYetImplemented()
}

#' @export
get_current_requests <- function() {

  if (!file.exists(request_path()))
    return(character(0))

  readRDS(request_path())
}

request_path <- function() {
  cache_dir <- get_cache_dir()
  file.path(cache_dir, "requests.rds")
}

refresh_requests <- function() {

  requests <- get_current_requests()
  failures <- get_current_failures()

  # ignore nodes we've already marked as failures
  requests <- setdiff(requests, failures)
  already_in_cache <- vapply(requests, in_cache, logical(1L))

  incomplete_requests <- unique(requests[!already_in_cache])
  saveRDS(incomplete_requests, request_path())
}

add_request <- function(user_ids) {

  current_requests <- get_current_requests()
  all_requests <- c(current_requests, user_ids)

  saveRDS(all_requests, request_path())
  refresh_requests()  # remove reduplicates and completes
}

failure_path <- function() {
  cache_dir <- get_cache_dir()
  file.path(cache_dir, "failures.rds")
}

#' @export
get_current_failures <- function() {

  if (!file.exists(failure_path()))
    return(character(0))

  readRDS(failure_path())
}

add_failure <- function(user_ids) {

  current_failures <- get_current_failures()
  all_failures <- c(current_failures, user_ids)

  saveRDS(all_failures, failure_path())
  refresh_failures()  # remove reduplicates and completes
}

refresh_failures <- function() {

  failures <- get_current_failures()
  sampled <- vapply(failures, in_cache, logical(1L))

  still_dont_have <- unique(failures[!sampled])
  saveRDS(still_dont_have, failure_path())
}

#' Slate users for addition to the cache
#'
#' @param screen_names A (character?) vector of screen names or user ids
#'   to slate for addition to the cache.
#' @param neighborhood Logical indicating whether the neighbors of each
#'   user should be added as well. Neighbors include both friends and
#'   followers. Defaults to `FALSE`.
#'
#' @return Nothing, called for side effects
#' @export
#'
#' @include visit.R
#'
#' @examples
#'
#' \dontrun{
#'
#' users <- c("blahasjaf80920930", "tim", "alexpghayes", "alexgphayes")
#' request(users, neighborhood = TRUE)
#' get_current_requests()
#'
#' }
#'
request <- function(screen_names, neighborhood = FALSE) {

  create_cache_if_needed()
  token <- get_registered_token(1)

  found <- lookup_users(screen_names, token = token)
  not_found <- setdiff(screen_names, found$screen_name)

  if (length(not_found) > 0)
    for (missing in not_found)
      warning(glue("Could not find user: {missing}", sep = "\n"), call. = FALSE)

  new_nodes <- found$user_id
  add_request(new_nodes)

  if (neighborhood) {
    for (node in new_nodes) {
      followers <- safe_get_followers(node, token = token)
      friends <- safe_get_friends(node, token = token)

      add_request(friends$to)
      add_request(followers$from)
    }
  }
}

