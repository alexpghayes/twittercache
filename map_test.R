node_data <- get_node_data()
edge_data <- get_edge_data()

library(dplyr)

map <- tibble(id64 = unique(c(edge_data$to, edge_data$from))) %>%
  mutate(id32 = row_number())

ed32 <- edge_data %>%
  left_join(map, by = c("to" = "id64")) %>%
  rename(to32 = id32) %>%
  left_join(map, by = c("from" = "id64")) %>%
  rename(from32 = id32) %>%
  select(to32, from32)

rm(edge_data)

gc()

tg <- tidygraph::as_tbl_graph(ed32)

pryr::object_size(tg)

library(tidygraph)

map2 <- map %>%
  mutate_at(vars(id32), as.character)

tg2 <- tg %>%
  activate(nodes) %>%
  left_join(map2, by = c("name" = "id32"))

pryr::object_size(tg2)
