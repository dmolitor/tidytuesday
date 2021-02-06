library(tidyverse)
library(wordcloud2)

avatar <- read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-11/avatar.csv'
)

# Clean up full_text variable
avatar <- avatar %>%
  mutate(full_text = str_replace_all(full_text,
                                     "[\\[\\]!.?,;'\"\\(\\)0-9]", 
                                     ""))

# Get wordcloud ready.
# Also remove "boring" words ;)
words <- tibble(
  word = unlist(str_split(avatar$full_text, pattern = " "))
) %>%
  mutate(
    word = tolower(word),
    word = trimws(word)
  ) %>%
  count(word) %>%
  rename(freq = n) %>%
  filter(!word %in% c("a", 
                      "and",
                      "but",
                      "or",
                      "the",
                      "of",
                      "to",
                      "you",
                      "me",
                      "she",
                      "her",
                      "hers",
                      "he",
                      "his",
                      "him",
                      "at",
                      "as",
                      "is",
                      "it",
                      "in",
                      "i",
                      "on",
                      "",
                      "\t",
                      "-",
                      "--")) %>%
  arrange(desc(freq))

image <- "~/last_airbender.jpg"

# Wordcloud in shape of aang
wordcloud2(words,
           figPath = image,
           size = 1,
           backgroundColor = "#282a36",
           color = rep(c("#ff79c6",
                         "#8be9fd",
                         "#f8f8f2",
                         "#bd93f9",
                         "#6272a4"),
                       nrow(words)),
           shuffle = FALSE,
           rotateRatio = 0.25)

# Standard wordcloud
wordcloud2(words,
           size = 1.5,
           backgroundColor = "#282a36",
           color = rep(c("#ff79c6",
                         "#8be9fd",
                         "#f8f8f2",
                         "#bd93f9",
                         "#6272a4"),
                       nrow(words)),
           shuffle = FALSE,
           rotateRatio = 0.25)

