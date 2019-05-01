library(tidyverse)
library(lubridate)

bird_collisions <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-30/bird_collisions.csv")

plot <- bird_collisions %>%
     mutate(year = lubridate::year(date)) %>%
     ggplot(aes(year)) +
     geom_bar(aes(fill = fct_infreq(habitat) %>% fct_rev()), 
              position = "stack") +
     geom_hline(yintercept = 0, color = "white") +
     scale_fill_manual(values = c("#FFF5EE", "#B5D7A6", "#197230")) +
     labs(x = "Year", y = "Number of Collisions",
          title = "Bird-Window Collisions in Chicago: 1978-present",
          subtitle = "Overall increase in the number of collisions, with forest-living birds showing a disproportionate increase",
          fill = "Natural Habitat:   ",
          caption = "Visualization: @frau_dr_barber
          Source: https://doi.org/10.1098/rspb.2019.0364") +
     theme(plot.background = element_rect(fill = "#B3DDF2"),
           panel.background = element_rect(fill = "#B3DDF2"),
           legend.background = element_rect(fill = "#B3DDF2"),
           axis.text = element_text(size = 12),
           axis.title = element_text(size = 14),
           plot.title = element_text(size = 14, face = "bold"),
           legend.position = c(0.12, 0.79),
           axis.ticks = element_blank(),
           panel.grid.major.x = element_blank(),
           panel.grid.minor.x = element_blank(),
           panel.grid.minor.y = element_blank()
           ) 

ggsave("bird_collisions.png", dpi = "retina", height = 5, width = 8, units = "in")
