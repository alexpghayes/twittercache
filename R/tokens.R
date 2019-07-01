#' Register a Twitter authentication token with twittercache
#'
#' If you try to register a token already in the cache,
#' you'll get a warning. That is, you are protected from
#' creating duplicate entries in the token cache.
#'
#' For more details on how to get tokens please read
#' `vignette("auth", package = "rtweet")`.
#'
#' @param token A Twitter auth token token created by
#'   [rtweet::create_token()].
#'
#' @export
#'
#' @family managing tokens
#'
#' @examples
#'
#' \dontrun{
#'
#' # if you haven't registered your token with
#' # rtweet yet, do this
#'
#' token <- rtweet::create_token(
#'   app = "FILL_THIS_IN",
#'   consumer_key = "FILL_THIS_IN",
#'   consumer_secret = "FILL_THIS_IN",
#'   access_token = "FILL_THIS_IN",
#'   access_secret = "FILL_THIS_IN"
#' )
#'
#' # HOWEVER, you can register the same token
#' # with rtweet multiple times by accident
#' # if you are not careful
#'
#' # to register tokens with twittercache that
#' # you have already registered with rtweet
#'
#' rtweet_tokens <- rtweet::get_tokens()
#'
#' for (token in rtweet_tokens)
#'   register_token(token)
#'
#' }
#'
register_token <- function(token) {

  create_cache_if_needed()
  token_dir <- file.path(get_cache_dir(), "tokens")

  num_tokens <- get_number_of_tokens()

  # make sure the token isn't a duplicate

  if (num_tokens > 0) {
    for (index in 1:num_tokens) {
      current_token <- get_registered_token(index)

      if (isTRUE(all.equal(token$credentials, current_token$credentials))) {
        warning("Token has already been registered.", call. = FALSE)
        invisible()
      }
    }
  }

  new_token_path <- file.path(token_dir, glue("{num_tokens + 1}.rds"))
  readr::write_rds(token, new_token_path)
}

#' Check how many tokens are in your cache
#'
#' @return An integer.
#' @export
#'
#' @family managing tokens
#'
get_number_of_tokens <- function() {
  create_cache_if_needed()
  token_dir <- file.path(get_cache_dir(), "tokens")
  length(list.files(token_dir))
}

#' Remove all registered tokens from the cache
#'
#' Will ask you for confirmation before proceeding because
#' of the how irritating it is to register tokens in the
#' first place.
#'
#' @export
#' @family managing tokens
#'
clear_tokens <- function() {

  if (usethis::ui_yeah("I want to remove all the tokens from my cache")) {

    token_dir <- get_token_dir()
    token_files <- list.files(token_dir, full.names = TRUE)

    for (file in token_files)
      file.remove(file)
  }
}

#' Get the path to the token directory
#'
#' Each token is saved in the token directory as:
#'
#' - `1.rds`
#' - `2.rds`
#' - ...
#'
#' @return Path to the token directory.
#'
#' @keywords internal
#'
get_token_dir <- function() {
  file.path(get_cache_dir(), "tokens")
}

#' Get a specific token in the cache
#'
#' Errors if there are no tokens, or the requested
#' token index is out of bounds.
#'
#' @return A Twitter auth token.
#'
#' @keywords internal
#'
get_registered_token <- function(index) {

  num_tokens <- get_number_of_tokens()

  if (num_tokens < 1)
    stop(
      "Couldn't find any tokens.",
      "Register your tokens with `register_token()`",
      call. = FALSE
    )

  if (index < 0 || index > num_tokens)
    stop("`index` must be between 1 and `get_number_of_tokens().", call. = FALSE)

  token_path <- file.path(get_token_dir(), glue("{index}.rds"))
  readr::read_rds(token_path)
}

#' Get all registered tokens in a list
#'
#' Errors if no tokens have been registered.
#'
#' @return A list containing all the registered tokens
#'
#' @keywords internal
#'
get_all_tokens <- function() {

  num_tokens <- get_number_of_tokens()

  if (num_tokens < 1)
    stop(
      "Couldn't find any tokens.",
      "Register your tokens with `register_token()`",
      call. = FALSE
    )

  token_dir <- get_token_dir()

  tokens <- list()

  for (i in 1:num_tokens) {
    token_path <- file.path(token_dir, glue("{i}.rds"))
    tokens[[i]] <- readr::read_rds(token_path)
  }

  tokens
}
