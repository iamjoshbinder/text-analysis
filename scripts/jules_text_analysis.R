# Objective: Read a novel from project gutenberg and then tidy it. 
# Thereafter, count the number of words, and perform basic sentiment analysis
# Script name: jules_text_analysis.R

library(gutenbergr)
library(tidytext)
library(tidyverse)
library(tm) # for Corpus()

# Show all gutenberg works in the gutenbergr package
gutenberg_works<- gutenberg_works(languages = "en")
View(gutenberg_works)

# The id for `Twenty Thousand Leagues Under the Sea by Jules Verne is 2488`

jules<- gutenberg_download(2488)

# now lets grab the novel 
jules_tidy<- jules %>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words, by="word")%>%

  # Lets create a custom theme
mytheme<- theme_bw()+
  theme(plot.title = element_text(color = "darkred"))+
  theme(panel.border = element_rect(color = "steelblue", size = 2))+
  theme(plot.title = element_text(hjust = 0.5)) # where 0.5 is to center

jules_tidy %>%
  count(word, sort = TRUE) %>%
  filter(n >100  & n<400) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  #xlab(NULL) +
  coord_flip()+
  mytheme+
  ggtitle("Top words in Jules Verne book ")