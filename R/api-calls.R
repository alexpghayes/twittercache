
# experimental attempt at respecting rate limits. two issues:
#
#   - somehow the waiting logic is incorrect and it doesn't
#     sleep when it should
#
#   - massive efficiency losses for large numbers of tokens
#     in wait time if sequentially asking for the rate limit
#     remainders before asking for data
#
#  inspired by code from @fchen365
#
find_token <- function(query = "friends/ids") {

  tokens <- get_all_tokens()

  limits <- rtweet::rate_limits(tokens) %>%
    dplyr::filter(query == !!query)

  calls_remaining <- any(limits$remaining > 0)

  # wait the shortest amount of time for the query to reset
  if (!calls_remaining) {
    min_until_reset <- min(limits$reset)
    message("API calls exhausted. Waiting ", min_until_reset * 60, " seconds.")
    Sys.sleep(min_until_reset * 60)
    return(find_token(query))
  }

  # return the token with most remaining calls
  index <- which.max(limits$remaining)
  tokens[[index]]
}



#' Check rate limits for registered tokens
#'
#' There should be no need to do this, but it may make you feel better.
#'
#' @export
check_rate_limits <- function() {

  tokens <- get_all_tokens()

  rtweet::rate_limits(tokens) %>%
    dplyr::filter(query %in% c("friends/ids", "followers/ids", "users/lookup"))

}

# probably should give these like 5 attempts or something like
# that just in case

# TODO: fancy sampling
safe_get_friends <- function(node, token = NULL, ...) {

  stopifnot(length(node) == 1)

  friends <- rtweet::get_friends(node, token = token, verbose = FALSE, ...)

  if (nrow(friends) == 0)
    return(empty_edge_data)

  colnames(friends) <- c("from", "to")
  friends
}

safe_get_followers <- function(node, token = NULL, ...) {

  stopifnot(length(node) == 1)

  followers <- rtweet::get_followers(node, token = token, verbose = FALSE, ...)

  if (all(is.na(followers$user_id)))
    return(empty_edge_data)

  colnames(followers) <- "from"
  followers$to <- node

  followers
}

safe_lookup_users <- function(user_id, attempts) {

  token <- find_token("users/lookup")

  for (i in 1:attempts) {

    user_data <- tryCatch({
      rtweet::lookup_users(users = user_id, token = token)
    }, error = function(cond) {
      NULL
    }, warning = function(cond) {
      NULL
    })

    if (!is.null(user_data))
      break
  }

  user_data
}
