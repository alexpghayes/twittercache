library(neo4r)
#
# system("docker stop neo4j")
# system("docker rm neo4j")

system("docker run --name neo4j --rm --env NEO4J_AUTH=neo4j/password --publish=7474:7474 --publish=7687:7687 -d neo4j:3.5.21")

docker run --name neo4j --env NEO4J_AUTH=none --publish=7474:7474 --publish=7687:7687 neo4j

# docker toolbox works a little differently, does not point to localhost

con <- neo4j_api$new(
  url = "http://192.168.99.100:7474",
  user = "neo4j",
  password = "password"
)

con$ping()

#' Adds a user to the database
#'
#' @param username the name of the user to be added
#' @param con the neo4r connexion object used to connect to the database
#' @param friend_depth the number of the user's friends to request; the rtweet default is 5000
#'
#' @return An nx2 tibble where n is the number of friends that the user has stored in the db
add_to_db <- function(username, con, friend_depth=5000) {
  list <- paste0('MATCH (n:User) WHERE n.username = "', username, '" RETURN n') %>% call_neo4j(con)
  if(length(list) > 0) {
    return(list$n)
  }

  friends <- get_friends(username, n=friend_depth)
  if(length(friends) == 0)
    return(NULL) # user doesn't exist or rate limit reached

  # Resolve the user_ids to screen names
  friends <- lookup_users(friends$user_id)

  # Collapse the list into a string to be used in the Neo4J call
  friends_string <- paste0('\'', paste0(friends$screen_name, collapse='\',\''), '\'')

  # Add the user to the db
  user <- paste0('CREATE (n:User {username:\'', username, '\', friends:[', friends_string, ']}) RETURN n', collapse='\',\'') %>%
    call_neo4j(con)

  # Create links from <the new user> to <anyone who the new user follows that is already in the db>
  paste0('MATCH (a:User {username:"', username, '"}),(b:User) WHERE (any(x IN a.friends WHERE x = b.username) AND NOT EXISTS((a)-[:Follows]->(b))) CREATE (a)-[r:Follows]->(b)') %>%
    call_neo4j(con)

  # Create links from <anyone who is already in the db and following the new user> to <the new user>
  paste0('MATCH (a:User {username:"', username, '"}),(b:User) WHERE any(x IN b.friends WHERE x = a.username) CREATE (b)-[r:Follows]->(a)') %>%
    call_neo4j(con)


  return(user)
}

con$ping()
con$get_version()
con$get_constraints()
con$get_labels()
con$get_relationships()
con$get_index()

str(con)

play_movies() %>%
  call_neo4j(con)


# so for a tabular database, i just need to point to the SQLite file

system("docker stop neo4j && sleep 2 && docker rm neo4j")
