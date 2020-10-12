
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
