# Second example - automatically generating a presentation

# libraries we will be using
library(tidyverse)
library(officer)
library(patchwork)

# read the data
friends <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends.csv')
friends_info <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends_info.csv')

# some function definitions

# plot themes
theme_set(saridr::theme_sarid() + 
            theme(plot.title = element_text(size = 10),
                  plot.subtitle = element_text(size = 8),
                  axis.text.x = element_text(size = 7),
                  axis.text.y = element_text(size = 7)))

# Common characters (including guests)
common_appearances <- friends %>% 
  count(speaker, sort = T) %>% 
  filter(n >= 200) %>% 
  filter(!is.na(speaker)) %>% 
  filter(!(speaker %in% c("#ALL#", "Scene Directions")))

# distribution of spoken words
spoken_words_distribution <- friends %>% 
  filter(speaker %in% common_appearances$speaker) %>% 
  mutate(words = str_count(text, pattern = " ") + 1) %>% 
  group_by(speaker, season, episode) %>% 
  group_by(speaker, season, episode) %>% 
  summarize(total_words = sum(words))

# common words used

tokenized_words <- 
  friends %>% 
  filter(speaker %in% common_appearances$speaker) %>% 
  tidytext::unnest_tokens(output = words, input = text) %>% 
  count(speaker, words, sort = T) %>% 
  filter(!(words %in% tidytext::stop_words$word)) %>% 
  group_by(speaker) %>% 
  slice_max(order_by = n, n = 10)

# The following function will create a nice chart showing data on the character
# Example:
create_character_charts("Chandler Bing")

create_character_charts <- function(current_speaker) {
  
  character_tibble <- spoken_words_distribution %>% 
    filter(speaker == current_speaker)
  
  words_distribution_plot <- ggplot(character_tibble, aes(total_words)) + 
    geom_density(fill = saridr::sarid_colors$light_blue_gradient) + 
    xlab("Words per episode") + 
    ggtitle("Words per episode distribution (density function)") + 
    labs(caption = "Example by Sarid Research Institute https://www.sarid-ins.co.il")
  
  peak_words_per_episode <- character_tibble %>% 
    ungroup() %>% 
    dplyr::slice_max(total_words, n=1)
  
  episode_data <- friends_info %>% 
    filter(season == peak_words_per_episode$season,
           episode == peak_words_per_episode$episode)
  
  words_per_episode <- 
    ggplot(character_tibble, 
           aes(y = season, x = episode, fill = total_words)) + 
    geom_tile() + 
    scale_fill_viridis_c() + 
    ggtitle(paste0(current_speaker, "'s words per episode"),
            subtitle = paste0("Max episode: ", 
                              episode_data$title, 
                              " S", episode_data$season,
                              "E", episode_data$episode)) + 
    scale_y_continuous(breaks = 1:10) + 
    scale_x_continuous(breaks = seq(1, 25, by = 2)) + 
    coord_cartesian(xlim = c(1,25))
  
  common_words_for_character <- tokenized_words %>% 
    filter(speaker == current_speaker) %>% 
    ggplot(aes(y = words, x = n)) + 
    geom_col(fill = saridr::sarid_colors$light_blue_gradient) + 
    ggtitle("Commonly used words") + 
    xlab("Number of appearances (in entire show)") + 
    labs(caption = "based on data from tidytuesday, see: https://github.com/rfordatascience/tidytuesday")
    
  
  (words_per_episode / words_distribution_plot) | (common_words_for_character)
  
}

# Add the actual slides ----

presentation_raw <- read_pptx("Templates/Template.pptx") %>%
  add_slide(master = "master_face",
            layout = "face") %>%
  ph_with(location = ph_location_label("title"),
          value = "Friends characters") %>%
  ph_with(location = ph_location_label("sub_title"),
          value = as.character(Sys.Date()))

presentation <- presentation_raw

walk(common_appearances$speaker,
     ~{
       presentation <<- presentation %>% 
         saridr::add_slide_plot(slide_id = paste0("character: ", .x),
                        myplot = create_character_charts(.x),
                        title = paste0("Showing data for ", .x), 
                        layout = "content_slide", master = "master_content_eng")
     }
     )

# Export the final presentation
print(presentation, target = "exports/tmp_draft_presentation.pptx")
