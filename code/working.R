logo <- png::readPNG("uswnt.png")
logo <- grid::rasterGrob(logo, interpolate = TRUE) 

usa_blue <- "#1D2642"
usa_red <- "#BB222E"

wwc_outcomes %>%
     mutate(
          place = case_when(
               round == "Final" & win_status == "Won" ~ 1,
               round == "Final" & win_status == "Lost" ~ 2,
               round == "Third Place Playoff" & win_status == "Won" ~ 3,
               round == "Third Place Playoff"& win_status == "Lost" ~ 4,
               round == "Quarter Final" & win_status == "Lost" ~ 5
          )
     ) %>%
     filter(!is.na(place)) %>%
     mutate(highlight = if_else(team == "USA", "yes", "no")) %>% 
     ggplot(aes(year, place, group = team)) +
     #annotation_custom(logo) +
     geom_line(aes(color = highlight), size = 2) +
     geom_point(aes(color = highlight), size = 5) +
     #geom_point(color = "white", size = 1) +
     annotate("text", x = 2021, y = 1, label = "USA") +
     labs(x = "\nWorld Cup Year",
          y = "",
          title = "The US women's team is the winningest team in world cup history",
          subtitle = "Despite this, they make dramatically less than male players - even though
          they win more games and generate more revenue"
     ) +
     #geom_text(data = . %>% filter(year == 2019, place < 5), aes(label = team, x = 2021)) +
     scale_x_continuous(breaks = c(1991, 1995, 1999, 2003, 2007, 2011, 2015, 2019),
                        expand = c(.05, .05)) +
     annotation_custom(logo, xmin = 2020, xmax = 2022, ymin = 4, ymax = 5) +
     scale_y_continuous(trans = "reverse",
                        labels = c("1st", "2nd", "3rd", "4th",
                                   "5th-8th\n(Quarter Finals)")) +
     scale_color_manual(values = c("gray", usa_red)) +
     #annotation_custom(logo, xmin = 2020, xmax = 2021, ymin = 1, ymax = 1) +
     #coord_cartesian(ylim = c(1:5))
     theme_minimal() +
     theme(legend.position = "none",
           panel.grid.major.y = element_blank(),
           panel.grid.minor = element_blank(),
           text = element_text(color = usa_blue, family = "Avenir Next Condensed Medium"),
           plot.margin = margin(0, 1, 0, 0, unit = "cm"))
