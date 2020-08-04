library(tidyverse)
library(viridis)

types <- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-04/energy_types.csv"
)

# Get 3-year totals for each type of energy by country
types_total <- types %>%
  mutate(across(country_name, replace_na, "United Kingdom")) %>%
  group_by(country_name) %>%
  mutate(id = cur_group_id(),
         angle = 90 - 360 * (
           (id - 0.5)/(n_distinct(types$country)+2) # angle for plot labels
         ),
         hjust = case_when(angle < -90 ~ 1,
                           TRUE        ~ 0), # hjust for plot labels
         angle = case_when(angle < -90 ~ angle + 180,
                           TRUE        ~ angle)) %>%
  rowwise() %>%
  mutate(total = sum(c_across(`2016`:`2018`)))

# Hack code so there is a gap in the bar plot for the scale labels
types_total[297:298, ] <- NA
types_total[297:298, 8] <- c(38, 39)

# Plot bar graph. geom_hlines are for circular y-axis
# and annotate command sets the labels for that axis
ggplot(types_total) +
  geom_hline(yintercept = 100000,
             color = viridis(n = 8,
                             option = "plasma",
                             direction = -1)[1],
             size = .05,
             alpha = .4) +
  geom_hline(yintercept = 300000,
             color = viridis(n = 8,
                             option = "plasma",
                             direction = -1)[2],
             size = .05,
             alpha = .4) +
  geom_hline(yintercept = 600000,
             color = viridis(n = 8,
                             option = "plasma",
                             direction = -1)[3],
             size = .05,
             alpha = .4) +
  geom_hline(yintercept = 1200000,
             color = viridis(n = 8,
                             option = "plasma",
                             direction = -1)[4],
             size = .05,
             alpha = .4) +
  geom_hline(yintercept = 1800000,
             color = viridis(n = 8,
                             option = "plasma",
                             direction = -1)[5],
             size = .05,
             alpha = .4) +
  geom_bar(aes(x = factor(id),
               y = total,
               fill = type),
           stat = "identity") +
  coord_polar() +
  ylim(-1000000, 2000000) +
  geom_text(data = types_total %>%
              group_by(id) %>%
              mutate(total = sum(total)) %>%
              ungroup() %>%
              distinct(id, .keep_all = TRUE),
            aes(x = factor(id),
                y = total + 100000,
                label = country_name,
                hjust = hjust,
                angle = angle),
            inherit.aes = FALSE,
            color = "white") +
  annotate("text", 
           x = factor(rep(max(types_total$id), 5)), 
           y = c(100000, 300000, 600000, 1200000, 1800000), 
           label = c("100k GWh", 
                     "300k GWh", 
                     "600k GWh", 
                     "1.2M GWh", 
                     "1.8M GWh"),
           color="gray",
           size=2.5,
           fontface="bold",
           hjust = .7) +
  theme_minimal() +
  labs(title = "Energy Production (2016-2018)") +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.title = element_blank(),
        legend.position = c(.08, .1),
        legend.text = element_text(color = "white"),
        axis.ticks = element_blank(),
        plot.background = element_rect(color = "black",
                                       fill = "black"),
        title = element_text(color = "white",
                             face = "bold")) +
  scale_fill_viridis(option = "plasma",
                     discrete = TRUE,
                     direction = 1)
