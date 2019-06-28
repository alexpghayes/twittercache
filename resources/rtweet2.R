## ------------------------------------------------------------
## what: customize rtweet functions to allow multiple tokens
## when: 04/03/2018
## who: fan chen (fan.chen@wisc.edu)
## ------------------------------------------------------------
library(rtweet)

## ------------------------------------------------------------
## download friend list
## ------------------------------------------------------------
download_friends <- function(id,
                             tokens = get_tokens(),
                             ...) {
  token = find_token(tokens, 'friends/ids')
  tryCatch({
    get_friends(users = id, token = token, ...)$user_id #, retryonratelimit = TRUE, parse = FALSE
  }, error = function(cond) {
    message(cond, " (get ", id, " friends)")
    return(character())
  }, warning = function(cond) {
    warning(cond, " (get ", id, " friends)")
    return(character())
  }, finally = {
    ## left blank intentionally
  })
}

## ------------------------------------------------------------
## download friend details
## ------------------------------------------------------------
get_user_data <- function(friends,
                          tokens = get_tokens(),
                          ...) {
  token = find_token(tokens, 'users/lookup')
  tryCatch({
    lookup_users(users = friends, token = token, ...)
  }, error = function(cond) {
    message(cond, " (get user data)")
    return (data.frame())
  }, warning = function(cond) {
    warning(cond, " (get user data)")
    return (data.frame())
  }, finally = {
    ## left blank intentionally
  })
}

## ------------------------------------------------------------
## download timeline
## ------------------------------------------------------------
download_timeline <- function(user, n,
                              tokens = get_tokens(),
                              try.max = 5,
                              ...) {
  ## try.max - number of re-attemp

  for (i in 1:try.max) {
    token = find_token(tokens, 'statuses/user_timeline', length(user))
    res = tryCatch({
      get_timeline(user = user, n = n, check = F, token = token, ...)
    }, error = function(cond) {
      message(cond, " (get ", user, " timeline)")
      return (data.frame())
    }, warning = function(cond) {
      warning(cond, " (get ", user, " timeline)")
      return (data.frame())
    }, finally = {
      ## left blank intentionally
    })

    if (!is.null(res) && nrow(res))
      break
  }
  res
}

## ------------------------------------------------------------
## find token
## ------------------------------------------------------------
find_token = function(tokens = get_tokens(),
                      query = 'friends/ids',
                      rate = 1,
                      break_time = 2,
                      strategy = 'random') {
  ## query - type of API request
  ## rate - rate limit to ask (for use)
  ## break_time - time interval to wait, in minutes
  ## strategy - 'random' for uniformly at random,
  ##            'seq' for in ascending order
  found = F
  while (!found) {
    if (strategy == 'random') {
      seq_find = sample(seq_len(length(tokens)))
    } else if (strategy == 'seq')
      seq_find = seq_len(length(tokens))

    for (i in seq_find) {
      remaining = tryCatch({
        rate_limit(tokens[[i]], query = query)$remaining
      }, error = function(cond) {
        message(cond, " (find token)")
        return(0)
      }, warning = function(cond) {
        warning(cond, " (find token)")
        return(0)
      }, finally = {
        ## left blank intentionally
      })
      if (is.null(remaining) || !length(remaining)) remaining = 0
      if (remaining > rate) break
    }

    if (remaining > rate) {
      token = tokens[[i]]
      found = T
    } else {
      message('All tokens are exhausted; let them breath... (', break_time,
              ' mins) at ', Sys.time())
      Sys.sleep(60 * break_time)
    }
  }
  token
}
