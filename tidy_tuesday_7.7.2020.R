library(tidyverse)
library(ggplot2)
library(GGally)
library(RColorBrewer)

coffee <- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv"
)

# Drop outlier (probably data entry error) and drop unneeded cols
coffee <- coffee %>%
  filter(total_cup_points > 0)
# Which coffee gets higher points typically?
ggplot(coffee) +
  geom_boxplot(aes(x = factor(species),
                   y = total_cup_points,
                   color = factor(species))) +
  coord_cartesian(ylim = c(60, 100)) +
  labs(x = "Species", y = "Total Cup Points") +
  theme(panel.grid.major = element_blank(),
        legend.title = element_blank())

# How is altitude related to species?
ggplot(coffee) +
  geom_boxplot(aes(x = factor(species),
                   y = altitude_mean_meters,
                   color = factor(species))) +
  coord_cartesian(ylim = c(0, 4000)) +
  labs(x = "Species", y = "Altitude") +
  theme(panel.grid.major = element_blank(),
        legend.title = element_blank())

# How is processing related to quality?
ggplot(coffee %>%
         filter(!is.na(processing_method))) +
  geom_boxplot(aes(x = fct_reorder(factor(processing_method),
                                   total_cup_points,
                                   .fun = "median"),
                   y = total_cup_points,
                   color = factor(processing_method))) +
  labs(x = "Processing Method", y = "Total Cup Points") +
  theme(panel.grid.major = element_blank(),
        legend.title = element_blank()) +
  coord_flip()

# What are the characteristics of both types of coffee?
coffee_characteristics <- coffee %>%
  mutate_at(vars(c(aroma:sweetness, moisture)), scale) %>%
  group_by(species) %>%
  summarise_at(vars(c(aroma:sweetness, moisture)), mean) %>%
  pivot_longer(cols = aroma:moisture,
               names_to = "quality",
               values_to = "Scaled\nRating")

# heatmap
colors <- brewer.pal(n = 3, name = "BrBG")

ggplot(coffee_characteristics) +
  geom_tile(aes(x = species, y = quality, fill = `Scaled\nRating`)) +
  scale_fill_gradient2(midpoint = -1.5,
                       low = colors[1],
                       mid = colors[2],
                       high = colors[3]) +
  theme(axis.title = element_blank(),
        legend.title = element_text(""),
        panel.background = element_blank())

# How do the qualities relate to each other?
plotFn <- function(data, mapping, method = "lm", ...) {
  p <- ggplot(data = data, mapping = mapping) +
    geom_point(alpha = 0.1, color = "lightsteelblue3") +
    geom_smooth(method = method, color = "lightsalmon3", se = FALSE)
  p
}

ggpairs(coffee,
        columns = c(21:29, 31),
        lower = list(continuous = wrap(plotFn)),
        upper = list(continuous = wrap(plotFn)),
        axisLabels = "internal") +
  theme(panel.background = element_blank())
