library(plotly)
library(tidyverse)

penguins_raw <- read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-28/penguins_raw.csv'
)

measures <- penguins_raw %>%
  mutate(`Body Mass (g)` = as.numeric(`Body Mass (g)`)/1000,
         `Flipper Length (mm)` = as.numeric(`Flipper Length (mm)`)/100,
         `Culmen Length (mm)` = as.numeric(`Culmen Length (mm)`)/10,
         `Culmen Depth (mm)` = as.numeric(`Culmen Depth (mm)`)/10) %>%
  group_by(Island) %>%
  summarise("Body Mass\n(kg)" = mean(`Body Mass (g)`, na.rm = TRUE),
            "Bill Length (cm)" = mean(`Culmen Length (mm)`, na.rm = TRUE),
            "Bill Depth\n(cm)" = mean(`Culmen Depth (mm)`, na.rm = TRUE),
            "Flipper Length (cm/10)"= mean(`Flipper Length (mm)`, na.rm = TRUE)) %>%
  pivot_longer(cols = `Body Mass\n(kg)`:`Flipper Length (cm/10)`,
               names_to = "measure",
               values_to = "value")

plot_ly(
  type = "scatterpolar",
  mode = "lines",
  fill = "toself",
  line = list(
    color = "#f8f8f2"
  )
) %>%
  add_trace(
    r = measures %>% filter(Island == "Biscoe") %>% pull(value),
    theta = measures %>% filter(Island == "Biscoe") %>% pull(measure),
    name = "Biscoe",
    fillcolor = "#ff79c6",
    opacity = .8,
    line = list(
      color = "#ff79c6"
    )
  ) %>%
  add_trace(
    r = measures %>% filter(Island == "Dream") %>% pull(value),
    theta = measures %>% filter(Island == "Dream") %>% pull(measure),
    name = "Dream",
    fillcolor = "#8be9fd",
    opacity = .8,
    line = list(
      color = "#8be9fd"
    )
  ) %>%
  add_trace(
    r = measures %>% filter(Island == "Torgersen") %>% pull(value),
    theta = measures %>% filter(Island == "Torgersen") %>% pull(measure),
    name = "Torgersen",
    fillcolor = "#bd93f9",
    opacity = .8,
    line = list(
      color = "#bd93f9"
    )
  ) %>%
  layout(
    title = list(
      text = "Penguin Size",
      size = 3,
      color = "#bd93f9",
      x = 0.012,
      y = 0.988
    ),
    polar = list(
      radialaxis = list(
        visible = TRUE,
        range = c(0, 5),
        showgrid = TRUE,
        showline = FALSE,
        color = "#f8f8f2",
        tickcolor = "#f8f8f2",
        tickvals = c(0, 1, 2, 3, 4, 5)
      ),
      bgcolor = "#282a36"
    ),
    paper_bgcolor  = "#282a36",
    font = list(
      color = "#f8f8f2"
    )
  )
