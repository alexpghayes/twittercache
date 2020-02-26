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

  create_cache_if_needed()

  con <- get_cache_db_connection()
  on.exit(dbDisconnect(con))

  tbl(con, "failed") %>%
    collect()
}

#' Empty the failure log
#'
#' @export
#' @family managing failures
#'
clear_failures <- function() {

  create_cache_if_needed()

  con <- get_cache_db_connection()
  on.exit(dbDisconnect(con))

  # TODO: set indices as in create_cache_if_needed()
  dbWriteTable(con, "failed", empty_failed_queries, overwrite = TRUE)
}

#' Re-request users in the failure log
#'
#' Also empties the failure log.
#'
#' @export
#' @family managing failures
#'
rerequest_failures <- function() {
  failed_queries <- get_failures()$query
  clear_failures()

  for (q in failed_queries) {
    if (!users_in_cache(q))
      add_users_to_cache(q)
  }
}

add_users_to_failed <- function(users) {

  new_failed <- tibble::tibble(query = users)

  con <- get_cache_db_connection()
  on.exit(dbDisconnect(con))

  dbWriteTable(con, "failed", new_failed, append = TRUE)

  invisible()
}

# user could be a user_id or a screen_name here
# example of failure PutinRF_Eng
failed_to_sample_users <- function(users) {
  users %in% get_failures()$query
}
#
# u <- c("PutinRF_Eng", "alexpghayes")
#
# failed_to_sample_users(u)


# user could be a user_id or a screen_name here
# inelegant (rewrites entire failure table instead of removing rows)
# but whatever
remove_from_failed <- function(user) {

  new_failed <- get_failures() %>%
    filter(!(query %in% user))

  con <- get_cache_db_connection()
  on.exit(dbDisconnect(con))

  dbWriteTable(con, "failed", new_failed, overwrite = TRUE)

  invisible()
}
