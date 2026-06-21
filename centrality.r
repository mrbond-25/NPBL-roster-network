library(tidyverse)
library(igraph)

edges <- read_csv("edgelist.csv", show_col_types = FALSE) %>%
  select(from = Poke.x, to = Poke.y, weight)

g <- graph_from_data_frame(edges, directed = FALSE) %>%
  igraph::simplify(edge.attr.comb = "first")

eigen_centrality <- eigen_centrality(g, weights = E(g)$weight)$vector %>%
  enframe(name = "Pokemon", value = "EigenCentrality") %>%
  arrange(desc(EigenCentrality))

betweenness <- betweenness(g, weights = 1 / E(g)$weight, normalize = TRUE) %>%
  enframe(name = "Pokemon", value = "Betweenness") %>%
  arrange(desc(Betweenness))

df_centrality <- eigen_centrality %>%
  inner_join(betweenness, by = "Pokemon")

k_cores <- coreness(g) %>%
  enframe(name = "Pokemon", value = "Core") %>%
  arrange(desc(Core))
df_centrality <- df_centrality %>%
  inner_join(k_cores, by = "Pokemon") %>%
  arrange(desc(Core))

write_csv(df_centrality, "centrality_measures.csv")
