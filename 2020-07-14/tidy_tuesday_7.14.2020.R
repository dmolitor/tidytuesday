library(choroplethr)
library(choroplethrMaps)
library(ggmap)
library(ggplot2)
library(magrittr)
library(maps)
library(RColorBrewer)
library(tidyverse)

astronauts <- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv"
)

# Clean country names
astronauts %<>%
  mutate(
    nationality       = case_when(
      nationality == "Republic of South Africa" ~ "South Africa",
      nationality == "U.K./U.S."                ~ "United Kingdom",
      nationality == "U.S.S.R/Russia"           ~ "Russia",
      nationality == "U.S.S.R/Ukraine"          ~ "Ukraine",
      nationality == "UAE"                      ~ "United Arab Emirates",
      nationality == "U.S."                     ~ "United States of America",
      nationality == "U.K."                     ~ "United Kingdom",
      nationality == "Czechoslovakia"           ~ "Czech Republic",
      nationality == "Hungry"                   ~ "Hungary",
      nationality == "Korea"                    ~ "South Korea",
      nationality == "Malysia"                  ~ "Malaysia",
      nationality == "Netherland"               ~ "Netherlands",
      TRUE                                      ~ nationality
    ),
    nationality_chor = tolower(nationality),
    nationality_map   = case_when(
      nationality == "United Kingdom"           ~ "UK",
      nationality == "United States of America" ~ "USA",
      TRUE                                      ~ nationality
    )
  )

# Get choroplethr map data
data("country.map")

# Choropleth of country of origin
country_count <- astronauts %>%
  distinct(nationality_chor, 
           name, 
           .keep_all = TRUE) %>%
  count(nationality_chor) %>%
  rename(region = nationality_chor,
         value  = n)

country_choropleth(country_count,
                   num_colors = 8,
                   zoom = sort(unique(country.map$region))[-5],
                   title = "Astronauts by Nationality") +
  theme_void() +
  theme(panel.grid = element_blank(),
        legend.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()) +
  labs(x = "",
       y = "") +
  scale_fill_brewer(palette = "YlOrRd") +
  theme(legend.title = element_blank())

# More detailed Choropleth
country_count <- astronauts %>%
  distinct(nationality_map, 
           name, 
           .keep_all = TRUE) %>%
  count(nationality_map) %>%
  rename(region = nationality_map,
         value  = n) %>%
  full_join(map_data("world"),
            by = "region")

ggplot(country_count) +
  geom_polygon(aes(x = long, 
                   y = lat,
                   group = group,
                   fill = value)) +
  coord_map(xlim = c(-180, 180),
            ylim = c(-50, 90)) +
  scale_fill_distiller(
    palette = "YlOrRd",
    direction = 1,
    values = c(0, .005, .015, .03, .1, .15, .25, .4, .6, .8, 1),
    na.value = "black"
  ) +
  theme_dark() +
  theme(panel.grid = element_blank(),
        legend.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()) +
  labs(x = "",
       y = "",
       title = "Astronauts by Nationality")
