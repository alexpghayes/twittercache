library(socialsampler)

#'
#'
#' Note:
#'   - Make sure (not in this function) to add failed user
#'   searches to the DB w/ attribute failed=TRUE
get_friends <- function(users, ...) {
  existing_users <- get_nodes() %>%
    extract2(1) %>%
    rename(from=username, to=friends) %>%
    summarise(across(.fns=as.character)) %>%
    select(from, to)

  users[users %in% existing_users$from] %>%
    socialsampler::safe_get_friends() %>%
    bind_rows(existing_users)
}

format_neo4j <- function(x) x %>%
  extract2(1) %>%
  rename(from=username) %>%
  summarise(across(.fns=as.character)) %>%
  select(from)
