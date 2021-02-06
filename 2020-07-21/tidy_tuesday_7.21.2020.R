library(cowplot)
library(ggplot2)
library(tidyverse)
library(viridis)

animal_outcomes <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/animal_outcomes.csv')

# Pivot animal_outcomes longer
animal_outcomes_long <- animal_outcomes %>%
  pivot_longer(cols = ACT:WA,
               names_to = "region",
               values_to = "count") %>%
  mutate(region = case_when(region == "ACT" ~ "Australian Capital Territory",
                            region == "NSW" ~ "New South Wales",
                            region == "NT"  ~ "Northern Territory",
                            region == "QLD" ~ "Queensland",
                            region == "SA"  ~ "South Australia",
                            region == "TAS" ~ "Tasmania",
                            region == "VIC" ~ "Victoria",
                            region == "WA"  ~ "Western Australia")) %>%
  group_by(outcome) %>%
  mutate(outcome_n = sum(count, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(region) %>%
  mutate(region_n = sum(count, na.rm = TRUE)) %>%
  ungroup()

ggplot(animal_outcomes_long %>%
         mutate(outcome = factor(outcome,
                                 levels = animal_outcomes_long %>%
                                   distinct(outcome, .keep_all = TRUE) %>%
                                   arrange(desc(outcome_n)) %>%
                                   pull(outcome),
                                 ordered = TRUE))) +
  geom_bar(aes(x = factor(year),
               y = count,
               fill = outcome),
           stat = "identity") +
  scale_fill_viridis(option = "plasma",
                     discrete = TRUE,
                     direction = 1) +
  labs(x = "", y = "", title = "Australian Animal Outcomes") +
  theme_minimal_hgrid() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 90),
        plot.title = element_text(face = "bold",
                                  hjust = 0.5))

ggplot(animal_outcomes_long %>%
         mutate(outcome = factor(outcome,
                                 levels = animal_outcomes_long %>%
                                   distinct(outcome, .keep_all = TRUE) %>%
                                   arrange(desc(outcome_n)) %>%
                                   pull(outcome),
                                 ordered = TRUE),
                region = factor(region,
                                levels = animal_outcomes_long %>%
                                  distinct(region, .keep_all = TRUE) %>%
                                  arrange(desc(region_n)) %>%
                                  pull(region),
                                ordered = TRUE))) +
  geom_bar(aes(x = factor(year),
               y = count,
               fill = outcome),
           position = "fill",
           stat = "identity") +
  facet_wrap(~ region,
             ncol = 2) +
  scale_fill_viridis(option = "plasma",
                     discrete = TRUE,
                     direction = 1) +
  labs(x = "", y = "", title = "Australian Animal Outcomes") +
  theme_minimal_hgrid() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 90),
        plot.title = element_text(face = "bold",
                                  hjust = 0.5))
