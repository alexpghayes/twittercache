library(neo4r)
library(rtweet)
library(tidyverse)
library(magrittr)

con <- neo4j_api$new(
  url = "http://localhost:7474",
  user = "neo4j",
  password = "password"
)


#' The same as call_neo4j(...) but warnings are suppressed
sup4j <- function(query, con) {
  suppressWarnings(call_neo4j(query, con))
}

n_friendsfollowers = 150


#' @param users A character vector of user ids (never screen names)
#'
#' @return A tibble where each row corresponds to a User and each column
#' to one of the User properties. If a user cannot be sampled, should
#' return nothing for that user. If no users can be sampled, should
#' return an empty tibble with appropriate columns.
#'
lookup_users <- function(user_ids) {
  user_data <- db_lookup_users(user_ids)
  not_in_graph <- setdiff(user_ids, user_data$user_id)
  new_user_data <- update_users(not_in_graph, lookup=TRUE)

  not_sampled <- user_data %>%
                 filter(is.na(sampled_at))
  if(dim(not_sampled)[1] == 0)
    upgraded_user_data <- empty_lookup()
  else
    upgraded_user_data <- not_sampled %>%
                          pull(user_id) %>%
                          update_users(lookup=TRUE)

  user_data <- user_data %>%
               filter(!is.na(sampled_at))

  bind_rows(user_data, new_user_data, upgraded_user_data)
}


#' @return an empty tibble with columns named after all of the lookup_user
#' properties.  Used when user data is unavailabale
empty_lookup <- function() {
  tibble(
    user_id='.',
    screen_name='.',
    protected=FALSE,
    followers_count=0,
    friends_count=0,
    listed_count=0,
    statuses_count=0,
    favourites_count=0,
    account_created_at='.',
    verified=FALSE,
    profile_url='.',
    profile_expanded_url='.',
    account_lang=FALSE,
    profile_banner_url='.',
    profile_background_url='.',
    profile_image_url='.',
    name='.',
    location='.',
    description='.',
    url='.',
    sampled_at='.',
    sampled_friends_at='.',
    sampled_followers_at='.'
  ) %>% slice(0L)
}


#' @return an empty 2-column tibble used as a placeholder for
#' when user data is not available
empty_user_edges <- function() {
  tibble(from='.', to='.') %>% slice(0L)
}


#' @param user_ids list of user_ids to fetch existing lookup_user data for in
#' the db
#'
#' @return a tibble with any existing data for user_ids
db_lookup_users <- function(user_ids) {
  # return a tibble where each row corresponds to a User and each column
  # to one of the User properties. when a user in not present in the
  # database, should not return a row in the output tibble for that
  # user. if no users are in the db should return an empty tibble with one
  # column for each User property
  user_ids <- c(user_ids)
  data <- paste('MATCH (n) WHERE n.user_id in ["',
                 paste(user_ids, collapse='","'),
                 '"] RETURN n', sep='') %>%
          sup4j(con)

  if(length(data) == 0)
    empty_lookup()
  else
    data %>%
      extract2(1) %>%
      bind_rows(empty_lookup())
}


#' @param lookup whether or not data should be gathered from rtweet::lookup_users
#'               for these users. Should only be false when empty users are being
#'               added to the DB (e.g. users that are friends with a user who
#'               *was* looked up)
#'
#' @return The tibble of user data, with one row for each (accessible)
#' user in `users` and one column for each property of `User` nodes
#' in the graph database.
update_users <- function(user_ids, sample_size=150, lookup=FALSE, get_friends=FALSE, get_followers=FALSE) {
  # make sure to set sampled_at to Sys.time() and
  # sampled_friends_at and sampled_followers_at to NULL
  # return data on users
  user_ids <- c(user_ids)
  if(length(user_ids) == 0)
    return(empty_lookup())

  if(lookup) {
    user_info <- rtweet::lookup_users(user_ids)
  } else {
    user_info <- empty_lookup() %>%
      bind_rows(tibble(user_id=user_ids))
  }

  if(length(user_info) == 0)
    return(empty_lookup())

  properties <- c('screen_name', 'protected', 'followers_count', 'friends_count',
                  'listed_count', 'statuses_count', 'favourites_count', 'account_created_at', 'verified', 'profile_url',
                  'profile_expanded_url', 'account_lang', 'profile_banner_url', 'profile_background_url', 'profile_image_url',
                  'name', 'location', 'description', 'url')
  prop_types <- c('chr', 'bool', 'num', 'num', 'num', 'num', 'num', 'chr', 'bool', 'chr',
                  'chr', 'chr', 'chr', 'chr', 'chr', 'chr', 'chr', 'chr', 'chr')
  nodes <- empty_lookup()
  for(i in seq(1, dim(user_info)[1])) {
    info <- user_info[i,]
    create_node <- paste('MERGE (n:User {user_id:"', info$user_id, '"}) SET ',
                         'n.sampled_at=',
                         if(lookup)
                           paste('"', Sys.time(), '"', sep='')
                         else
                           "NULL",
                         ',', sep='')

    for(j in seq(1, length(properties))) {
      if(is.na(info[[properties[j]]]))
        next

      create_node <- paste(create_node, 'n.', properties[j], '=', sep = '')

      if(prop_types[j] == 'chr')
        create_node <- paste(create_node, '"', info[[properties[j]]], '",', sep = '')
      else
        create_node <- paste(create_node, info[[properties[j]]], ',', sep = '')
    }

    new_node <- paste(substr(create_node, 1, nchar(create_node)-1), ' RETURN n', sep = '') %>%
                sup4j(con)

    if(length(new_node) != 0) {
      nodes <- nodes %>% bind_rows(new_node$n)
      if(get_friends) {
        db_connect_friends(info$user_id, sample_size)
      }
      if(get_followers) {
        db_connect_followers(info$user_id, sample_size)
      }
    }
  }

  nodes
}


#' Gets the friends for the given user_id and creates the edges in the graph.
#'
#' @param user_id the user_id of a SINGLE user who is already in the database and
#'                does not have friend edge data
#'
#' @return a nx2 tibble where the <from> column is user_id and the <to> column
#'         is the user_id of user_id's friends
db_connect_friends <- function(user_id, sample_size) {
  friends <- rtweet::get_friends(user_id, n=sample_size)
  sup4j(paste('MATCH (n:User {user_id:"', user_id, '"}) SET n.sampled_friends_at="', Sys.time(), '"', sep=''),
        con)

  results <- NULL
  for(user in friends$user_id) {
    temp <- sup4j(paste('MERGE (from:User {user_id:"', user_id, '"}) MERGE (to:User {user_id:"', user,
                        '"}) MERGE (from)-[r:FOLLOWS]->(to)', sep=''),
                  con)
    results <- results %>%
      bind_rows(tibble(from=user_id, to=user))
  }

  if(length(results) != 2)
    return(empty_user_edges())

  tibble(from=user_id, to=friends$user_id)
}


#' @param user_id a SINGLE user_id to create edges for
#' @param sample_size total number of followers to fetch for user_id
#'
#' @return a 2-column tibble edge list from all of user_id's followers to
#' user_id
db_connect_followers <- function(user_id, sample_size) {
  friends <- rtweet::get_followers(user_id, n=sample_size)
  sup4j(paste('MATCH (n:User {user_id:"', user_id, '"}) SET n.sampled_followers_at="', Sys.time(), '"', sep=''),
        con)

  results <- NULL
  for(user in friends$user_id) {
    temp <- sup4j(paste('MERGE (to:User {user_id:"', user_id, '"}) MERGE (from:User {user_id:"', user,
                        '"}) MERGE (from)-[r:FOLLOWS]->(to)', sep=''),
                  con)
    results <- results %>%
      bind_rows(tibble(from=user_id, to=user))
  }

  if(length(results) != 2)
    return(empty_user_edges())

  tibble(from=user_id, to=friends$user_id)
}


#' Gets the friends for the given user that already exist in the DB.
#'
#' @param user_ids a list of user_ids who are already in the DB and
#' already have friend edge data
#'
#' @return a 2-column tibble edge list with entries from the users in user_ids
#' to their friends
db_get_friends <- function(user_ids) {
  results <- paste('MATCH (from:User),(to:User) WHERE from.user_id in ["',
                   paste(user_ids, collapse='","'),
                   '"] AND (from)-[:FOLLOWS]->(to) RETURN from.user_id, to.user_id', sep='') %>%
    sup4j(con)

  if(length(results) != 2)
    return(empty_user_edges())

  tibble(from=results$from.user_id$value, to=results$to.user_id$value)
}


#' @param user_ids a list of user_ids who are already in the DB and
#' already have follower edge data
#'
#' @return a 2-column tibble edge list with entries from the followers of
#' the users in user_ids to the users in user_ids
db_get_followers <- function(user_ids) {
  results <- paste('MATCH (from:User),(to:User) WHERE to.user_id in ["',
                     paste(user_ids, collapse='","'),
                     '"] AND (from)-[:FOLLOWS]->(to) RETURN from.user_id, to.user_id', sep='') %>%
             sup4j(con)

  if(length(results) != 2)
    return(empty_user_edges())

  tibble(from=results$from.user_id$value, to=results$to.user_id$value)
}


#' @param user_ids A character vector of user ids (never screen names)
#'
#' @return A tibble where each row corresponds to a follower relationship
#' from the user in the 'from' column to the user in to 'to' column
get_followers <- function(user_ids, sample_size=150) {
  user_ids <- c(user_ids)
  status <- follower_sampling_status(user_ids)

  new_edges <- add_new_followers(status$not_in_graph, sample_size)
  upgraded_edges <- add_new_followers(status$sampled_friends_at_is_null, sample_size)
  existing_edges <- db_get_followers(status$sampled_friends_at_not_null)

  bind_rows(new_edges, upgraded_edges, existing_edges)
}


#' @param user_ids A character vector of user ids (never screen names)
#'
#' @return A tibble where each row corresponds to a follower relationship
#' from the user in the 'from' column to the user in to 'to' column
get_friends <- function(user_ids, sample_size=150) {
  # here we will need to query twice: once to ask who we actually
  # have *complete* friendship edges for, and then a second time to get
  # those friendship edges
  user_ids <- c(user_ids)
  status <- friend_sampling_status(user_ids)


  # sample the friends of all the users w/o sampled friends
  new_edges <- add_new_friends(status$not_in_graph, sample_size)
  upgraded_edges <- add_new_friends(status$sampled_friends_at_is_null, sample_size)
  existing_edges <- db_get_friends(status$sampled_friends_at_not_null)

  # need to be careful about duplicate edges here. ideally
  # we guarantee that edges are unique somehow before this, but if not
  # we can use dplyr::distinct(), although this is an expensive operation

  # TODO: I believe that these edges should all be duplicate free, but this
  #       needs to be verified
  bind_rows(new_edges, upgraded_edges, existing_edges)
}


#' @param user_ids a list of user_ids to add friend edges to the db for
#' @param sample_size how many friends to sample at a time for each user
#'
#' @return a 2-column tibble edge list from user_ids to their friends
add_new_friends <- function(user_ids, sample_size) {
  # set sampled_friends_at to Sys.time()
  # sampled_at and sampled_followers_at default to NULL
  # return friends of each user
  user_ids <- c(user_ids)

  if(is.na(user_ids))
    return(empty_user_edges())

  update_users(user_ids, lookup=FALSE)

  tbl <- NULL
  for(user_id in user_ids)
    tbl <- tbl %>%
      bind_rows(db_connect_friends(user_id))

  tbl
}


#' @param user_ids a list of user_ids to add follower edges to the db for
#' @param sample_size how many friends to sample at a time for each user
#'
#' @return a 2-column tibble edge list from respective followers to user_ids
add_new_followers <- function(user_ids, sample_size) {
  # set sampled_followers_at to Sys.time() and
  # sampled_at and sampled_friends_at default to NULL
  # return followers of each user
  user_ids <- c(user_ids)

  if(is.na(user_ids))
    return(empty_user_edges())

  update_users(user_ids, lookup=FALSE)

  tbl <- NULL
  for(user_id in user_ids)
    tbl <- tbl %>%
      bind_rows(db_connect_followers(user_id))

  tbl
}


#' @param user_ids to fetch the sampling status for
#'
#' @return a list of all users who either (1) are not currently in the
#' graph, (2) are in the graph but their followers have not been sampled,
#' (3) are in the graph and have sampled followers
follower_sampling_status <- function(user_ids) {
  # generate based on queries of user.sampled_followers_at node property
  present_users <- db_lookup_users(user_ids)
  not_in_graph <- setdiff(user_ids, present_users$user_id)
  unsampled_users <- present_users %>%
    filter(is.na(sampled_followers_at)) %>%
    pull(user_id)
  sampled_users <- setdiff(user_ids, c(unsampled_users, not_in_graph))

  if(length(not_in_graph) == 0)
    not_in_graph <- NA
  if(length(unsampled_users) == 0)
    unsampled_users <- NA
  if(length(sampled_users) == 0)
    sampled_users <- NA

  list(
    not_in_graph = not_in_graph,
    sampled_friends_at_is_null = unsampled_users,
    sampled_friends_at_not_null = sampled_users
  )
}


#' @param user_ids to fetch the sampling status for
#'
#' @return a list of all users who either (1) are not currently in the
#' graph, (2) are in the graph but their friends have not been sampled,
#' (3) are in the graph and have sampled friends
friend_sampling_status <- function(user_ids) {
  # generate based on queries of user.sampled_friends_at node property
  present_users <- db_lookup_users(user_ids)
  not_in_graph <- setdiff(user_ids, present_users$user_id)
  unsampled_users <- present_users %>%
    filter(is.na(sampled_friends_at)) %>%
    pull(user_id)
  sampled_users <- setdiff(user_ids, c(unsampled_users, not_in_graph))

  if(length(not_in_graph) == 0)
    not_in_graph <- NA
  if(length(unsampled_users) == 0)
    unsampled_users <- NA
  if(length(sampled_users) == 0)
    sampled_users <- NA

  list(
    not_in_graph = not_in_graph,
    sampled_friends_at_is_null = unsampled_users,
    sampled_friends_at_not_null = sampled_users
  )
}

