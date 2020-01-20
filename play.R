library(RSQLite)
library(rtweet)
library(tidyverse)

##### create the database ------------------------------------------------------

user <- "alexpghayes"

user_data <- lookup_users(user)

user_id <- user_data$user_id

friends <- safe_get_friends(user_id, token = get_token())
followers <- safe_get_followers(user_id, token = get_token())

edges <- friends %>%
  bind_rows(followers) %>%
  mutate(
    sampled_at = Sys.time()
  )

# create the database
cache_path <- get_cache_path()
con <- dbConnect(RSQLite::SQLite(), cache_path)

# create the user/node table
flattened_user_data <- flatten(user_data)

user_data_no_lists <- user_data %>%
  select_if(~!is.list(.x))

dbWriteTable(con, "nodes", user_data_no_lists, overwrite = TRUE)

# create the edge table
dbWriteTable(con, "edges", edges, overwrite = TRUE)

# create the failures table

suspended_user <- "PutinRF_Eng"
failed <- data.frame(query = suspended_user)

# create the edge table
dbWriteTable(con, "failed", failed, overwrite = TRUE)

dbDisconnect(con)

##### add friends and foes to the database -------------------------------------

library(dplyr)
con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "tmp.sqlite")

dbWriteTable(con, "mtcars", mtcars)

mtcars_tbl <- tbl(con, "mtcars")

# no assignment
mtcars_tbl %>%
  bind_rows(mtcars)

dbWriteTable(con, "mtcars", mtcars, append = TRUE)

?db_write_table()

copy_to(
  con, nycflights13::flights, "mtcars",
  temporary = FALSE
)

flights_db <- tbl(con, "flights")


