# these functions heavily inspired by code from @fchen365

check_rate_limits <- function() {

  tokens <- get_all_tokens()

  rtweet::rate_limits(tokens) %>%
    dplyr::filter(query %in% c("friends/ids", "followers/ids", "users/lookup"))

}

find_token <- function(query = "friends/ids") {

  tokens <- get_all_tokens()

  limits <- rtweet::rate_limits(tokens) %>%
    dplyr::filter(query == !!query)

  calls_remaining <- any(limits$remaining > 0)

  # wait the shortest amount of time for the query to reset
  if (!calls_remaining) {
    minutes_until_reset <- min(limits$reset)
    message(glue("API calls exhausted. Waiting {minutes_until_reset * 60} seconds."))
    Sys.sleep(minutes_until_reset * 60)
    return(find_token(query))
  }

  # return the token with most remaining calls
  index <- which.max(limits$remaining)
  tokens[[index]]
}

safe_get_friends <- function(node, ...) {

  stopifnot(length(node) == 1)

  token <- find_token("friends/ids")
  friends <- rtweet::get_friends(node, token = token, verbose = FALSE, ...)

  if (nrow(friends) == 0)
    return(empty_edgelist())

  colnames(friends) <- c("from", "to")
  friends
}

safe_get_followers <- function(node, ...) {

  stopifnot(length(node) == 1)

  token <- find_token("followers/ids")
  followers <- rtweet::get_followers(node, token = token, verbose = FALSE, ...)

  if (all(is.na(followers$user_id)))
    return(empty_edgelist())

  colnames(followers) <- "from"
  followers$to <- node

  followers
}

safe_lookup_user <- function(node, ...) {
  stopifnot(length(node) == 1)
  token <- find_token("users/lookup")
  rtweet::lookup_users(node, token = token)
}
