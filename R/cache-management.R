

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

  if (sys_path == "")use
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


