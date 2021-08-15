# A short visualization of the Friend's social network.
library(tidyverse)
friends <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends.csv')

all_characters <- friends %>% 
  filter(saridr::notin(speaker, c("Scene Directions", "#ALL#"))) %>% 
  filter(!is.na(speaker)) %>% 
  count(speaker, sort = T)

top_n_characters <- all_characters %>% 
  slice(1:200)

# ggplot(all_characters %>% 
#          mutate(x = seq_along(n)), 
#        aes(x = x, y = n)) + geom_line() + geom_point()
  
friends_scene_appearances <- friends %>% 
  filter(speaker %in% top_n_characters$speaker) %>% 
  distinct(speaker, season, episode, scene)

tot_scenes_per_season <- friends %>% 
  filter(speaker %in% top_n_characters$speaker) %>% 
  distinct(season, episode, scene) %>% 
  group_by(season, episode) %>%
  summarize(tot_scenes = max(scene)) %>% 
  group_by(season) %>% 
  summarize(tot_scenes = sum(tot_scenes))

# a function to retrieve the number of episodes
get_relative_pairity <- function(character1, character2) {
  friends_scene_appearances %>% 
    filter(speaker %in% c(character1, character2)) %>% 
    count(season, episode, scene) %>% 
    filter(n > 1) %>% 
    group_by(season) %>% 
    summarize(scene_appearances = n(), .groups = NULL) %>% 
    left_join(tot_scenes_per_season, by = "season") %>% 
    mutate(relative_pairity = scene_appearances/tot_scenes) %>% 
    select(season, relative_pairity)
}

# options(dplyr.summarise.inform = FALSE) # don't warn about groups after summarize
# 
# pb <- progress::progress_bar$new(format = "[:bar] :percent eta: :eta",
#                                  total = 19900)
# 
# characters_combo <- crossing(first_character = top_n_characters$speaker, 
#                              second_character = top_n_characters$speaker) %>% 
#   filter(first_character < second_character) %>%
#   mutate(scenes_together = map2(first_character, second_character,
#                                 ~{
#                                   pb$tick()
#                                   get_relative_pairity(.x, .y)
#                                 }))
#   
# 
# clean_characters_combo <- characters_combo %>% 
#   unnest(scenes_together)

# save the tibble (to save the runtime)
# write_csv(clean_characters_combo, "data/clean_characters_combo.csv")

# read the ready tibble ----
clean_characters_combo <- read_csv("data/clean_characters_combo.csv")


# Create as a graph -------------------------------------------------------

# Each character is an edge, each relative_parity is an edge 
# Edge thickness is relative_parity
library(network)
library(sna)

cleaner_network <- clean_characters_combo %>% 
  filter(relative_pairity > 0.005) %>% 
  filter(season == 1)

friends_graph <- network(cleaner_network %>% select(1:2) %>% as.matrix(),
                         directed = F)

match_vertex_attributes <- tibble(vertex.names = get.vertex.attribute(friends_graph, "vertex.names")) %>% 
  mutate(character_type = if_else(vertex.names %in% c("Rachel Green", "Ross Geller", "Chandler Bing",
                                                      "Monica Geller", "Joey Tribbiani", "Phoebe Buffay"),
                                  "main", 
                                  "secondary")) %>% 
  mutate(color = if_else(character_type == "main", "green", "red"))
  

set.vertex.attribute(friends_graph, "character_type",
                     match_vertex_attributes$character_type)

library(GGally)

ggnet(friends_graph)
