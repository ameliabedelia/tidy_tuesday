library(tidyverse)
library(paletteer)

wwc_outcomes <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/wwc_outcomes.csv")
logo <- png::readPNG("uswnt.png")
logo <- grid::rasterGrob(logo, interpolate = TRUE) 
usa_blue <- "#1D2642"
usa_red <- "#BB222E"
grey_palette <- paletteer_dynamic("cartography", "grey.pal", 17, direction = 1)
grey_palette <- grey_palette[1:11]

wwc_outcomes %>%
     mutate(
          place = case_when( #assign revere place order for graphing
               round == "Final" & win_status == "Won" ~ 5,
               round == "Final" & win_status == "Lost" ~ 4,
               round == "Third Place Playoff" & win_status == "Won" ~ 3,
               round == "Third Place Playoff"& win_status == "Lost" ~ 2,
               round == "Quarter Final" & win_status == "Lost" ~ 1
          )
     ) %>%
     filter(!is.na(place)) %>%
     mutate(highlight = if_else(team == "USA", "yes", "no")) %>% 
     ggplot(aes(year, place, group = team)) +
     annotation_custom(logo, xmin = 2003, xmax = 2007, ymin = 3.95, ymax = 4.95) +
     geom_line(aes(color = team), size = 1) +
     geom_point(aes(color = team), size = 6) +
     labs(x = "World Cup",
          y = "",
          title = "\nThe US Women's Team is the Winningest Team\nin World Cup History",
          subtitle = "The women's team is more successful and brings in more revenue than the men's team, 
          yet they are paid significantly less - around 38% of what the men make.",
          caption = "Source: data.world
          Visualization: Frau_Dr_Barber"
     ) +
     geom_text(data = . %>% filter(year == 2019, place > 1), 
               aes(label = team, x = 2021, family = "Roboto Condensed")) +
     scale_x_continuous(breaks = c(1991, 1995, 1999, 2003, 2007, 2011, 2015, 2019),
                        expand = c(.05, .05)) +
     scale_y_continuous(labels = c("5th-8th\n(Quarter Finals)",
                                   "4th", "3rd", "2nd", "1st")) +
     scale_color_manual(values = c(sample(grey_palette, 17, replace = TRUE), usa_red)) +
     theme_minimal() +
     theme(legend.position = "none",
           panel.grid.major.y = element_blank(),
           panel.grid.minor = element_blank(),
           text = element_text(color = usa_blue, family = "Roboto Condensed"),
           axis.text.x = element_text(size = 14),
           axis.text.y = element_text(size = 14),
           axis.title.x = element_text(size = 14),
           plot.title = element_text(size = 18, face = "bold"),
           plot.margin = margin(0, 1, 0, 0, unit = "cm"))

ggsave("wwc.png")
