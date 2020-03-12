library(RSQLite)
library(rtweet)
library(dplyr)
library(socialsampler)

##### create empty tables with the correct types -------------------------------

# populate initially with my own data, then create 0 row data frames
# from this data. see ?dbWriteTable

### nodes table ----------------------------------------------------------------

# TODO: add sampled_at column
# TODO: type of datetime objects should be non-integer

# TODO: set the indices for better performance

# created_at, sampled_at datetime type in the database?

user <- "alexpghayes"

node_data <- lookup_users(user) %>%
  mutate(
    sampled_at = Sys.time(),
    has_edges = FALSE
  ) %>%
  select_if(~!is.list(.x)) %>%
  select(screen_name, sampled_at, user_id, has_edges, created_at, everything())

empty_node_data <- node_data[0, ]

### edges table ----------------------------------------------------------------

# note: edges stored as from (user_id<chr>), to (user_id<chr>)
# never screen_names, never bit64

user_id <- node_data$user_id

friends <- safe_get_friends(user_id)

empty_edge_data <- friends[0, ]

### failed sampling table ------------------------------------------------------

failed <- tibble(query = user)
empty_failed_queries <- failed[0, ]

### add these empty tables to the internal package data ------------------------
usethis::use_data(
  empty_node_data,
  empty_edge_data,
  empty_failed_queries,
  internal = TRUE,
  overwrite = TRUE
)

