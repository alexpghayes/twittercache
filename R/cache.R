#' Check if a user is in the cache
#'
#' @param users A vector screen names or user IDs of Twitter users.
#'   All one or the other -- don't mix screen names and user IDs.
#'
#' @param has_edges If `NULL` returns whether the user is present in the
#'   cache, with or without edges. If `TRUE`, returns whether the user is
#'   present in the cache *and* additionally has edge data available.
#'   If `FALSE`, returns whether the user is present in the cache *and*
#'   their edge data is unavailable. Note that the user sets returned
#'   when `has_edges` is `TRUE` and `FALSE` are disjoint; setting `NULL`
#'   returns the union of these two sets.
#'
#' @return A logical vector.
#'
#' @export
#' @family cache management
#' @import dbplyr
#'
users_in_cache <- function(users, has_edges = NULL) {

  stopifnot(is.null(has_edges) || length(has_edges) == 1)
  stopifnot(is.null(has_edges) || is.logical(has_edges))

  create_cache_if_needed()

  con <- get_cache_db_connection()
  on.exit(dbDisconnect(con))

  if (!is.null(has_edges)) {
    matches <- tbl(con, "nodes") %>%
      filter(
        user_id %in% users | screen_name %in% users,
        has_edges == !!has_edges
      ) %>%
      select(user_id, screen_name) %>%
      collect()
  } else {
    matches <- tbl(con, "nodes") %>%
      filter(user_id %in% users | screen_name %in% users) %>%
      select(user_id, screen_name) %>%
      collect()
  }

  match_vector <- c(matches$user_id, matches$screen_name)

  users %in% match_vector
}

#' Add new users to the cache
#'
#' Notes:
#'
#'   - The calling function is responsible for making sure that the user
#'     isn't already in the cache
#'
#'   - If user A and user B are friends, and we add both user A
#'     and user B, we will duplicate any edges between A and B.
#'
#'   - Doesn't record information about protected accounts, treats these
#'     as failed queries.
#'
#'   - We only take the first 5,000 friends and 5,000 followers
#'     for the time being. The idea is that for important edges,
#'     the edge will get picked up from the other node. This may
#'     miss out on edges between node both with huge follower
#'     and following counts, but who cares about those.
#'
#' @param users
#'
#' @return
#' @export
#'
#' @importFrom dplyr filter select_if bind_rows select mutate everything
#' @import socialsampler
add_users_to_cache <- function(users, edges) {

  stopifnot(is.logical(edges))
  stopifnot(length(edges) == 1)

  if (length(users) < 1) {
    return(invisible())
  }

  # TODO: log this action

  # TODO: use safe_ version
  # if all the accounts are bad, might get NULL, or a data frame
  # with zero rows
  raw_user_data <- safe_lookup_users(users)

  # all the accounts are bad, we couldn't get user data for
  # any of them
  if (is.null(raw_user_data) || nrow(raw_user_data) < 1) {
    add_users_to_failed(users)
    return(invisible())
  }

  user_data <- raw_user_data %>%
    mutate(
      sampled_at = Sys.time(),
      has_edges = edges
    ) %>%
    select_if(
      ~!is.list(.x)
    ) %>%
    select(screen_name, sampled_at, user_id, has_edges, created_at, everything())

  # if we accidentally got information on a protected user,
  # ditch it
  protected <- user_data %>%
    filter(protected) %>%
    pull(user_id)

  if (length(protected) > 0)
    add_users_to_failed(protected)

  new_users <- user_data %>%
    filter(!protected)

  # add all of this information to the database
  con <- get_cache_db_connection()
  on.exit(dbDisconnect(con))

  dbWriteTable(con, "nodes", new_users, append = TRUE)

  if (edges) {

    # now we want to get the edges for each of the good new users
    # and store the edges using user_ids, not screen_name

    new_edges <- safe_get_followers(new_users$user_id) %>%
      bind_rows(safe_get_friends(new_users$user_id))

    dbWriteTable(con, "edges", new_edges, append = TRUE)
  }

  log_debug("Checking if successfully sampled users were previous failures")

  log_debug(glue("Number of users: {nrow(users)}"))
  log_debug(glue("Number of new users: {nrow(new_users)}"))
  log_debug(glue("Number of protected: {nrow(protected)}"))

  # input users
  # raw collected user data
  # unprotected users
  # protected users

  num_new_users <- nrow(new_users)

  # we may have attempted to sample these users before
  # and failed. if that is the case, update their failure
  # state

  if (num_new_users > 0) {
    for (index in 1:num_new_users) {

      # the original request could have been in terms of the user_id
      # or the screen name, so check them both

      user_id <- new_users$user_id[index]
      screen_name <- new_users$screen_name[index]

      log_debug(glue("Index: {index}"))
      log_debug(glue("User ID: {user_id}"))
      log_debug(glue("Screen name: {screen_name}"))

      if (failed_to_sample_users(user_id))
        remove_from_failed(user_id)

      if (failed_to_sample_users(screen_name))
        remove_from_failed(screen_name)
    }
  }

  invisible()
}

#' Update information on a user already in the cache to include edge data
#'
#' Notes:
#'
#'   - The calling function is responsible for making sure that the user
#'     is in the cache, but only node data is present, no edge data
#'
#'   - If user A and user B are friends, and we add both user A
#'     and user B, we will duplicate any edges between A and B.
#'
#'   - We only take the first 5,000 friends and 5,000 followers
#'     for the time being. The idea is that for important edges,
#'     the edge will get picked up from the other node. This may
#'     miss out on edges between node both with huge follower
#'     and following counts, but who cares about those.
#'
#' @param users
#'
#' @return
#' @export
#'
#' @importFrom dplyr filter select_if bind_rows select mutate everything
#' @import socialsampler
add_users_edge_data_to_cache <- function(users) {

  if (length(users) < 1) {
    return(invisible())
  }

  con <- get_cache_db_connection()
  on.exit(dbDisconnect(con))

  # potentially need to convert `screen_name` to `user_id`

  user_data <- tbl(con, "nodes") %>%
    filter(user_id %in% users | screen_name %in% users) %>%
    select(screen_name, user_id) %>%
    collect()

  # now we want to get the edges for each of the good new users
  # and store the edges using user_ids, not screen_name

  new_edges <- safe_get_followers(user_data$user_id) %>%
    bind_rows(safe_get_friends(user_data$user_id))

  dbWriteTable(con, "edges", new_edges, append = TRUE)

  # update the edge status for each node
  ## TODO: do this more efficiently!!

  new_node_data <- tbl(con, "nodes") %>%
    collect()

  new_node_data$has_edges[new_node_data$user_id %in% user_data$user_id] <- TRUE

  dbWriteTable(con, "nodes", new_node_data, overwrite = TRUE)

  invisible()
}

# remember to disconnect!
get_cache_db_connection <- function() {
  dbConnect(SQLite(), get_cache_path())
}

#' Remove suspended and deleted accounts from the cache
#'
#' - Also deduplicate edges
#'
#' Not yet implemented.
#'
#' @export
clean_cache <- function() {
  create_cache_if_needed()
  .NotYetImplemented()
}

get_cache_path <- function() {
  sys_path <- Sys.getenv("TWITTERCACHE_PATH")

  if (sys_path == "")
    return(path.expand("~/.twittercache.sqlite"))

  sys_path
}

# s <- sprintf("create table %s(%s, primary key(%s))", "DF",
#              paste(names(DF), collapse = ", "),
#              names(DF)[1])
# dbGetQuery(con, s)
# dbWriteTable(con, "DF", DF, append = TRUE, row.names = FALSE)

create_cache_if_needed <- function() {
  if (!cache_exists()) {

    log_debug(glue("No twittercache exists, creating one now."))

    con <- get_cache_db_connection()
    on.exit(dbDisconnect(con))

    # TODO: set indices
    #
    # https://stackoverflow.com/questions/6401583/set-or-create-primary-key-in-sqlite-from-r

    dbWriteTable(con, "nodes", empty_node_data)
    dbWriteTable(con, "edges", empty_edge_data)
    dbWriteTable(con, "failed", empty_failed_queries)
  }
}


#' Peak at the size of your Twittercache
#'
#' @return
#' @export
#'
#' @importFrom dplyr tbl count pull
print_cache <- function(count_edges = TRUE) {

  if (!cache_exists())
    stop("No twittercache detected.", call. = FALSE)

  con <- get_cache_db_connection()
  on.exit(dbDisconnect(con))

  num_nodes_by_edges <- tbl(con, "nodes") %>%
    count(has_edges) %>%
    pull(n)

  num_nodes_with_edges <- num_nodes_by_edges[2]
  num_nodes <- sum(num_nodes_by_edges)

  num_failed <- tbl(con, "failed") %>%
    count() %>%
    pull(n)

  # optional since slow
  if (count_edges) {
    num_edges <- tbl(con, "edges") %>%
      count() %>%
      pull(n)
  } else {
    num_edges <- "???"
  }

  glue(
    "Details about your twittercache\n",
    "\n",
    "  - {num_nodes} node(s) // {num_nodes_with_edges} with edge data\n",
    "  - {num_failed} failed sampling attempt(s)\n",
    "  - {num_edges} edge(s)",
    .trim = FALSE
  )
}

#' Remove all sampled users from the cache
#'
#' Will ask you for confirmation before proceeding because
#' of the how time consuming it is to sample users.
#'
#' @export
#' @family cache management
#'
delete_cache <- function() {

  if (!cache_exists())
    stop("No twittercache detected.", call. = FALSE)

  msg <- "I want to remove all Twitter user data from my cache"

  if (usethis::ui_yeah(msg))
    invisible(file.remove(get_cache_path()))
}

cache_exists <- function() {
  file.exists(get_cache_path())
}

#' @export
get_node_table <- function() {
  con <- get_cache_db_connection()
  on.exit(dbDisconnect(con))
  collect(tbl(con, "nodes"))
}

# may return duplicated edges. call clean_cache ahead of
# time to get nicer data
#' @export
get_edge_table <- function() {
  con <- get_cache_db_connection()
  on.exit(dbDisconnect(con))

  collect(tbl(con, "edges"))
}

