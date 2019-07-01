#' Get screen names from the failure log
#'
#' **NOTE**: calls `rtweet::lookup_users()` without checking API
#'   rate limits. Unless you have an enormous (90,000+) number of
#'   failures, or have been using your tokens outside of `twittercache`
#'   this is extremely unlikely to cause a problem.
#'
#' @return A character vector of screen names.
#' @export
#'
#' @family managing failures
#'
get_failures <- function() {
  ids <- get_failure_ids()

  if (length(ids) < 1)
    return(character(0))

  rtweet::lookup_users(ids, token = get_registered_token(1))$screen_name
}

#' Get the number of users in the failure log
#'
#' @return An integer.
#' @export
#'
#' @family managing failures
#'
get_number_of_failures <- function() {
  refresh_failures()
  length(get_failure_ids())
}

#' Empty the failure log
#'
#' @export
#' @family managing failures
#'
clear_failures <- function() {
  saveRDS(character(0), failure_path())
}

#' Re-request users in the failure log
#'
#' Also empties the failure log, because users can't be in both
#' the requests log and the failure log at the same time.
#'
#' @export
#' @family managing failures
#'
rerequest_failures <- function() {
  failures <- get_failure_ids()

  if (length(failures) < 1)
    stop("No failures in the failure log.", call. = FALSE)

  clear_failures()
  request(failures)
}

#' Get the path to the failure log
#'
#' The failure log is a character vector of user IDs (never
#' screen names) that gets stored as an `.rds` file.
#'
#' @return Path to `.rds` file.
#'
#' @keywords internal
#'
failure_path <- function() {
  cache_dir <- get_cache_dir()
  file.path(cache_dir, "failures.rds")
}

#' Add users to the failure log
#'
#' Also refreshes the failure log.
#'
#' @param user_ids One or most user IDS (*not* screen names) to add
#'   to the failure log.
#'
#' @keywords internal
#'
add_failure <- function(user_ids) {

  current_failures <- get_failure_ids()
  all_failures <- c(current_failures, user_ids)

  saveRDS(all_failures, failure_path())
  refresh_failures()
}

#' Update the failure log
#'
#' Removes duplicates and users that have been successfully
#' sampled from the failure log.
#'
#' @keywords internal
#'
refresh_failures <- function() {

  failures <- get_failure_ids()
  sampled <- vapply(failures, in_cache, logical(1L))

  still_dont_have <- unique(failures[!sampled])
  saveRDS(still_dont_have, failure_path())
}

#' Get user IDs from the failure log
#'
#' @return Character vector of users IDs that couldn't be
#'   sampled.
#'
#' @keywords internal
#'
get_failure_ids <- function() {

  if (!file.exists(failure_path()))
    return(character(0))

  readRDS(failure_path())
}
