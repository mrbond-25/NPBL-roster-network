library(igraph)
library(tidyverse)


#load dataset, remove whitespace and make team id for matching
df <- read.csv("raw_rosters.csv") %>% 
  filter(Poke != "") %>%
  mutate(team_id = paste(Season, Team)) %>%
  mutate(Poke = str_replace_all(Poke," ",""))

#make row for every teammate relationship
df2 <- df %>% inner_join(df,by = join_by(team_id), 
                         relationship = "many-to-many")

#summarize 
df3 <- df2 %>% group_by(Poke.x,Poke.y) %>%
  summarise(weight = n()) %>%
  filter(Poke.x != Poke.y)

#get data for each pokemon's edges
degs_data <- df3 %>% group_by(Poke.x) %>% 
  summarise(d= n(),m = mean(weight),s = sd(weight)) %>%
  rename(Pokemon = Poke.x) %>%
  filter(!(m==1 & s==0))

#compute Z scores for each
df4 <- df3 %>% inner_join(degs_data, join_by(Poke.x == Pokemon)) %>%
  inner_join(degs_data,join_by(Poke.y == Pokemon)) %>%
  mutate(z.x = (weight - m.x)/s.x, z.y = (weight - m.y)/s.y)

#remove edges if Z score for both are below 1.5
df_pruned <- df4 %>% filter(z.x >=1.5 | z.y >=1.5)

write.csv(df_pruned,"edgelist.csv")