library(ggplot2)
library(tidyverse)
library(viridis)

chopped <- read_tsv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-25/chopped.tsv"
)

chopped <- chopped %>%
  filter(!is.na(episode_rating)) %>%
  pivot_longer(judge1:judge3,
               names_to = "judge_no",
               values_to = "judge") %>%
  mutate(judge = str_replace_all(judge, 
                                 "Aarón Sanchez",
                                 "Aarón Sánchez"))
# Ratings by judge
judge_ratings <- chopped %>% 
  group_by(judge) %>% 
  summarise(mean_rating = mean(episode_rating, na.rm = T),
            n_ep = n()) %>% 
  arrange(desc(mean_rating)) %>%
  rename("Number of\nepisodes" = n_ep)

# Weighted avg of all ratings
wa <- sum(judge_ratings$mean_rating*judge_ratings$`Number of\nepisodes`)/
  sum(judge_ratings$`Number of\nepisodes`)

# Make plot
ggplot(judge_ratings) + 
  geom_hline(yintercept = wa, 
             color = "white", 
             linetype = "dashed") + 
  geom_bar(aes(x = factor(judge, levels = rev(unique(judge)), ordered = T), 
               y = mean_rating, 
               fill = `Number of\nepisodes`), 
           stat = "identity", 
           width = 1) + 
  geom_curve(aes(x = judge[83], 
                 y = wa + .5, 
                 xend = judge[83], 
                 yend = wa + .05), 
             color = "white",
             curvature = -.2, 
             arrow = arrow(angle = 45, 
                           length = unit(3, units = "mm"))) + 
  geom_curve(aes(x = judge[13], 
                 y = wa + .8, 
                 xend = judge[10], 
                 yend = wa + .5), 
             color = "white",
             curvature = .4, 
             arrow = arrow(angle = 45, 
                           length = unit(3, units = "mm"))) +
  annotate("text", 
           label = "Average IMDB rating\nfor all judges (8.38)", 
           color = "white",
           fontface = "bold",
           x = judge_ratings$judge[80], 
           wa + .8) + 
  annotate("text", 
           label = "A bunch of\none-hit wonders!", 
           color = "white", 
           fontface = "bold",
           x = judge_ratings$judge[15], 
           wa + .9) +
  scale_fill_viridis(option = "E", 
                     direction = 1) + 
  coord_flip() + 
  theme(panel.grid = element_blank(), 
        plot.background = element_rect(color = "#44475a", 
                                       fill = "#44475a"), 
        panel.background = element_rect(color = "#44475a", 
                                        fill = "#44475a"), 
        legend.background = element_rect(color = "#44475a", 
                                         fill = "#44475a"), 
        legend.text = element_text(color = "white",
                                   face = "bold"), 
        legend.title = element_text(color = "white",
                                    face = "bold"), 
        axis.text = element_text(color = "white",
                                 size = 8),
        axis.title = element_text(color = "white",
                                  face = "bold"),
        plot.title = element_text(color = "white",
                                  hjust = 0.5,
                                  face = "bold")) +
  labs(title = "Average IMDB Ratings for Judges",
       y = "IMDB Rating",
       x = "")
