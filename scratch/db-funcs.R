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
get_nodes <- function(attributes=NULL, type=NULL) {
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

  nodes <- noquote(paste0('MATCH (n',
                          if(is.null(type)) '' else paste0(':', type),
                          ' {',
                          attr_str,
                          '}) RETURN n',
                          sep="")
                   )

  # TODO: tidy up this tibble
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

  # TODO: tidy this tibble
  paste(added_node) %>%
    call_neo4j(con)
}
