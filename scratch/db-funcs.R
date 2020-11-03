library(neo4r)
library(rtweet)
library(tidyverse)
library(magrittr)

con <- neo4j_api$new(
  url = "http://localhost:7474",
  user = "neo4j",
  password = "password"
)


# Define an operator to more easily extract friends/followers dynamically
`%>>>%` <- function(a, condition)
  if(condition) a %>% use_series(friends) else a %>% use_series(followers)


#' Gets a node from the DB with attributes matching those in the list provided
#'
#' @return a tibble of matching nodes
db_get_node <- function(username=NULL, id=NULL, friends=NULL, followers=NULL, grab_friends=TRUE) {
  if(con$ping() != 200) {
    stop("Neo4J database cannot be reached.")
  }

  # TODO: Add variable safety checks
  with_phrase <- ""
  if(!is.null(friends)) {
    with_phrase <- paste0(if(!is.null(friends))
                            paste0('with ["', paste(friends, collapse='","'), '"] as friends '),
                          if(!is.null(followers))
                            paste0(', ["', paste(followers, collapse='","'), '"] as followers ')
                          )
  } else if(!is.null(followers)) {
    with_phrase <- paste0('with ["', paste(followers, collapse='","'), '"] as followers ')
  }

  match_phrase <- paste0(if(!is.null(username))
                           paste0(' and n.username in ["', paste(username, collapse='","'), '"]'),
                         if(!is.null(id))
                           paste0(' and n.id=', id),
                         if(!is.null(friends))
                           paste0(' and size([f in friends where f in n.friends])=size(friends)'),
                         if(!is.null(followers))
                           paste0(' and size([f in followers where f in n.followers])=size(followers)'),
                         ' '
                         )

  if(nchar(match_phrase) != 1)
    match_phrase <- paste0('WHERE ', substr(match_phrase, 6, nchar(match_phrase)))

  nodes <- noquote(paste0(with_phrase, 'MATCH (n:User) ', match_phrase, ' RETURN n'))

  print(nodes)

  # TODO: Make a check for no nodes returned before extract2ing
  nodes <- paste(nodes) %>%
    call_neo4j(con, type="graph")

  if(length(nodes) == 0)
    return(NULL)

  nodes <- nodes %>%
    extract2(1)

  # Format the data into a 2-column tibble with the data we care about (from and to)
  to_add <- nodes$properties[[1]] %>>>% grab_friends

  tib <- tibble(from=nodes$properties[[1]]$username,
                to=as.character(to_add))

  if(length(nodes$properties) < 2)
    return(tib)

  for(i in seq(2, length(nodes$properties), 1)) {
    to_add <- nodes$properties[[i]] %>>>% grab_friends
    if(is.null(to_add))
      next

    tib <- tib %>%
      add_row(from=nodes$properties[[i]]$username,
              to=as.character(to_add))
  }

  tib
}


#' Adds a SINGLE node to the DB
#'
#' @return a tibble containing the added node
add_node <- function(username, id, friends=NULL, followers=NULL,
                     friends_sampled_at=NULL, followers_sampled_at=NULL) {
  added_node <- paste0('MERGE (n:User {id:', id, '}) ON CREATE SET n.username = "', username, '"',
                       if(!is.null(friends))
                         paste0(' SET n.friends=["', paste(friends, collapse='", "'), '"], n.friends_sampled_at="', friends_sampled_at, '"'),
                       if(!is.null(followers))
                         paste0(' SET n.followers=["', paste(followers, collapse='", "'), '"], n.followers_sampled_at="', followers_sampled_at, '"'),
                       ' RETURN n'
                       )

  paste(noquote(added_node)) %>%
    call_neo4j(con, type="graph") %>%
    extract2(1)
}
