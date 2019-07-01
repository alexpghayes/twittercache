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

ig <- igraph::graph_from_data_frame(ed32)

e2
