# cases:
#  good users, already in the cache: return the data
#  bad users, already failed to sample them
#  good users, not in the cache yet
#  bad users, not in the cache yet

#' Title
#'
#' @param users
#'
#' @return
#' @export
#'
#' @importFrom dplyr collect distinct
cache_get_friends <- function(users) {

  existing_good <- users[users_in_cache(users)]
  existing_bad <- users[failed_to_sample_users(users)]

  new_users <- setdiff(users, c(existing_good, existing_bad))
  add_users_to_cache(new_users)

  con <- get_cache_db_connection()
  on.exit(dbDisconnect(con))

  nodes_tbl <- tbl(con, "nodes")
  edges_tbl <- tbl(con, "edges")

  good_user_ids <- nodes_tbl %>%
    filter(user_id %in% users | screen_name %in% users) %>%
    pull(user_id)

  if (length(good_user_ids) < 1) {
    return(empty_edge_data)  # TODO: avoid duplicate with empty_edgelist
  }

  edges_tbl %>%
    filter(from %in% good_user_ids) %>%
    distinct() %>%
    collect()
}

#' Title
#'
#' @param users
#'
#' @return
#' @export
#'
cache_get_followers <- function(users) {

  existing_good <- users[users_in_cache(users)]
  existing_bad <- users[failed_to_sample_users(users)]

  new_users <- setdiff(users, c(existing_good, existing_bad))
  add_users_to_cache(new_users)

  con <- get_cache_db_connection()
  on.exit(dbDisconnect(con))

  good_user_ids <- tbl(con, "nodes") %>%
    filter(user_id %in% users | screen_name %in% users) %>%
    pull(user_id)

  if (length(good_user_ids) < 1) {
    return(empty_edge_data)  # TODO: avoid duplicate with empty_edgelist
  }

  tbl(con, "edges") %>%
    filter(to %in% good_user_ids) %>% # only difference from cache_get_friends()
    distinct() %>%
    collect()
}

#' Title
#'
#' note: new users are gonna burn get_friends and get_followers tokens
#' so be careful here!
#'
#' @param users
#'
#' @return
#' @export
#'
cache_lookup_users <- function(users) {

  existing_good <- users[users_in_cache(users)]
  existing_bad <- users[failed_to_sample_users(users)]

  new_users <- setdiff(users, c(existing_good, existing_bad))
  add_users_to_cache(new_users)

  con <- get_cache_db_connection()
  on.exit(dbDisconnect(con))

  tbl(con, "nodes") %>%
    filter(user_id %in% users | screen_name %in% users) %>%
    collect()
}

