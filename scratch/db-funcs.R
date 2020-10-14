library(neo4r)
library(rtweet)

con <- neo4j_api$new(
  url = "http://localhost:7474",
  user = "neo4j",
  password = "password"
)


#' Gets a node from the DB with attributes matching those in the list provided
#'
#' @param attributes a list of attributes that the node should match
#' @param type a string designating the node type
#'
#' @return a tibble of matching nodes
get_nodes <- function(username=NULL, id=NULL, friends=NULL, followers=NULL) {
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

  print(nodes)

  # TODO: gonna have to use call_neo4j(con, type="graph").  The default call_neo4j
  #       gives a terrible return format

  paste(nodes) %>%
    call_neo4j(con)
}


#' Adds a node to the DB
#' TODO: add an edges parameter
#'
#' @param attributes a list of attributes to give the node
#' @param type a string designating the node type
#'
#' @return a tibble containing the added node
add_node <- function(attributes=NULL, type=NULL) {
  attr_str <- ""
  if(!is.null(attributes)) {
    for(i in seq(1, length(attributes))) {
      unlisted <- unlist(attributes[[i]])
      is_numeric <- is.numeric(unlisted)
      is_iterable <- is.list(attributes[[i]]) || is.vector(attributes[[i]]) ||
        is.array(attributes[[i]]) || is.table(attributes[[i]])
      attr_str <- paste(attr_str,
                        names(attributes)[[i]],
                        if(is_iterable) ':[' else ':',
                        if(!is_numeric) '"' else '',
                        paste(unlist(attributes[[i]]),
                              collapse=if(is.numeric(unlist(attributes[[i]]))) ',' else '","'),
                        if(!is_numeric) '"' else '',
                        if(is_iterable) ']' else '',
                        if(i != length(attributes)) ',' else '',
                        sep="")
    }
  }

  added_node <- noquote(paste0('CREATE (n',
                             if(is.null(type)) '' else paste0(':', type),
                             ' {',
                             attr_str,
                             '}) RETURN n',
                             sep="")
                      )

  # TODO: gonna have to use call_neo4j(con, type="graph").  The default call_neo4j
  #       gives a terrible return format

  paste(added_node) %>%
    call_neo4j(con)
}
