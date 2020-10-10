# A demo script for fun and important things you can do with R, for example
# figure out which character in "friends" comedy talks the most

# Scripts usually start by loading requried libraries, for instance, we will be using the tidyverse library
library(tidyverse)

# Then, we read the data, you can read it directly from online sources, as we do here (or from your hard drive)
friends <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends.csv')

# Now you can get to transforming, visualizing, and analyzing the data:
words_per_episode <- friends %>% 
  filter(speaker %in% 
           c("Rachel Green", "Ross Geller", "Chandler Bing", 
             "Monica Geller", "Joey Tribbiani", "Phoebe Buffay")) %>% 
  mutate(words = str_count(text, pattern = " ") + 1) %>% 
  group_by(speaker, season, episode) %>% 
  summarize(total_words = sum(words))

# Boxplot visualizations are quick to show the distribution

words_per_episode %>% 
  ggplot(aes(x = speaker, y = total_words)) + 
  geom_boxplot()

# Boxplots focus on the median and quartiles, we can also see the averages

words_on_average <- words_per_episode %>% 
  group_by(speaker) %>% 
  summarize(average_words = mean(total_words),
            sd_words = sd(total_words))