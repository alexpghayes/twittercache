library(socialsampler)
library(twittercache)
library(aPPR)



get_friends <- function(users, ...) {
  users <- c(users)
  if(length(users) == 0)
    return(invisible())

  existing_users <- db_get_node(username=users, grab_friends=TRUE)

  if(length(unique(existing_users$from)) != length(users)) {
    new_users <- users[!(users %in% existing_users$from)] %>%
      rtweet::get_friends() %>%
      rename(from=user, to=user_id)

    for(user in unique(new_users$from)) {
      # Add the user, do the checking for if the user already exists in
      # the add_node function itself
      info <- rtweet::lookup_users(user)
      if(info$protected) {
        # Skip protected users
        # TODO: Add protected users to the DB with some flag so that
        #       they don't get repeatedly searched for
        next
      }

      friends <- new_users$to[new_users$from == user]
      curr_date <- base::date()
      add_node(username=user, id=info$user_id, friends=friends, friends_sampled_at=curr_date)
    }
  }

  tryCatch({
    # Try to return the merged tibbles
    existing_users %>% bind_rows(new_users)
  }, error = function(e) {
    # We get an error if new_users doesn't exist
    existing_users
  })
}



get_followers <- function(users, ...) {
  users <- c(users)
  if(length(users) == 0)
    return(invisible())

  existing_users <- db_get_node(username=users, grab_friends=FALSE)

  if(length(unique(existing_users$to)) < length(users)) {
    new_users <- NULL

    # TODO: this boolean will have to be reworked because $from won't work anymore
    #       ((after reworking db_get_nodes()))
    for(user in users[!(users %in% existing_users$from)]) {
      new_users <- rtweet::get_followers(user) %>%
        bind_cols(to=user) %>%
        bind_rows(new_users)
    }
    new_users <- new_users %>%
      rename(from=user_id)

    for(user in unique(new_users$to)) {
      # Add the user, do the checking for if the user already exists in
      # the add_node function itself
      info <- rtweet::lookup_users(user)
      if(info$protected) {
        # Skip protected users
        # TODO: Add protected users to the DB with some flag so that
        #       they don't get repeatedly searched for
        next
      }

      followers <- new_users$from[new_users$to == user]
      curr_date <- base::date()
      add_node(username=user, id=info$user_id, followers=followers, followers_sampled_at=curr_date)
    }
  }

  tryCatch({
    # Try to return the merged tibbles
    existing_users %>% bind_rows(new_users)
  }, error = function(e) {
    # We get an error if new_users doesn't exist
    existing_users
  })
}




lookup_users <- function(users, ...) {
  users <- c(users)
  if(length(users) == 0)
    return(invisible())

  # TODO: reword the db_get_nodes function so that it works how we want it to now
  existing_users <- db_get_nodes(users=users)

  # TODO: this boolean will have to be reworked because $from won't work anymore
  #       ((after reworking db_get_nodes()))
  new_users <- users[!(users %in% existing_users$from)]
}






