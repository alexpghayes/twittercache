register_token <- function(token) {

  create_cache_if_needed()
  token_dir <- file.path(get_cache_dir(), "tokens")

  num_tokens <- get_number_of_tokens()

  # make sure the token isn't a duplicate
  if (num_tokens > 0) {
    for (index in 1:num_tokens) {
      current_token <- get_registered_token(index)

      if (isTRUE(all.equal(token$credentials, current_token$credentials))) {
        stop("Token has already been registered.", call. = FALSE)
      }
    }
  }

  new_token_path <- file.path(token_dir, glue("{num_tokens + 1}.rds"))
  saveRDS(token, new_token_path)
}

get_number_of_tokens <- function() {
  create_cache_if_needed()
  token_dir <- file.path(get_cache_dir(), "tokens")
  length(list.files(token_dir))
}

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

  token_dir <- file.path(get_cache_dir(), "tokens")
  token_path <- file.path(token_dir, glue("{index}.rds"))
  readRDS(token_path)
}
