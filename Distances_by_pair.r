library(tidyverse)
library(igraph)

df <- read.csv("full_edgelist.csv") %>%
  select(Poke.x, Poke.y, weight)

adj_matrix <- df %>%
  select(Poke.x, Poke.y, weight) %>%
  pivot_wider(names_from = Poke.y, values_from = weight, values_fill = 0) %>%
  column_to_rownames("Poke.x") %>%
  as.matrix()

row_sums <- rowSums(adj_matrix)
row_sums[row_sums == 0] <- 1
normalized_matrix <- adj_matrix / row_sums

distance_matrix <- as.matrix(dist(normalized_matrix, method = "euclidean"))

distance_df <- as.data.frame(distance_matrix) %>%
  rownames_to_column("Pokemon1") %>%
  pivot_longer(-Pokemon1, names_to = "Pokemon2", values_to = "Distance") %>%
  filter(Pokemon1 < Pokemon2) %>%
  arrange(Distance) %>%
  full_join(df, by = join_by(Pokemon1 == Poke.x, Pokemon2 == Poke.y)) %>%
  select(Pokemon1, Pokemon2, Distance, weight) %>%
  mutate(weight = ifelse(is.na(weight), 0, weight),
    percentile = percent_rank(Distance)) %>%
  arrange(Distance)

plot(distance_df$Distance, distance_df$weight, xlab = "Distance", ylab = "Weight", main = "Distance vs Weight")

write.csv(distance_df %>% head(2000), "Closest_Pairs.csv", row.names = FALSE)
