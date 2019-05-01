library(tidyverse)
library(ggthemes)

tidy_anime <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-23/tidy_anime.csv")

score_summary <- tidy_anime %>%
     filter(!is.na(studio)) %>% 
     group_by(studio) %>%
     summarise(
          n = n(),
          score_mean = mean(score, na.rm = TRUE),
          scored_by_mean = mean(scored_by, na.rm = TRUE),
          ) %>% 
     ungroup()

plot <- ggplot(score_summary, aes(n, score_mean)) +
     geom_point(aes(size = scored_by_mean), alpha = 0.7,
                show.legend = FALSE) +
     geom_smooth(se = FALSE, color = "dimgray") +
     scale_x_log10() +
     scale_color_viridis(option = "A") +
     labs(title = "Do Bigger Studios Make Higher Rated Anime?",
          subtitle = "Point size correlates with number of user ratings",
          x = "Number of Titles from Studio",
          y = "Average Rating",
          caption = "Data: MyAnimeList \nVisualization: @frau_dr_barber") +
     theme_solarized() +
     theme(plot.title = element_text(color = "black"),
          plot.subtitle = element_text(size = 9),
          plot.caption = element_text(size = 7))

ggsave("studio_rating.png", height = 4, width = 5, units = "in")
