library(tidyverse)
library(cowplot)
library(glue)

commute_mode <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-05/commute.csv")

commute_mode %>%
    filter(mode == "Bike", !is.na(state_region)) %>%
    mutate(
        fraction = percent / 100,
        city_size = glue::glue("{city_size}
                               cities"),
        state_region = fct_reorder(state_region, fraction)
    ) %>% 
    ggplot(aes(city_size, fraction)) +
    geom_boxplot(aes(fill = city_size), notch = TRUE, outlier.size = 1,
                 color = "white", show.legend = FALSE) +
    scale_y_continuous(trans = "log10",
                       labels = scales::percent) +
    facet_wrap(vars(fct_rev(state_region)), nrow = 1) +
    labs(x = "", y = NULL,
         title = "Nobody Bikes to Work in the US - less than 1%!",
         subtitle = "More people cycle to work in bigger cities and in the western US. Ridership is equally low throughout the south.",
         caption = glue::glue("Source: ACS - US Census
                              Visualization @frau_dr_barber")) +
    cowplot::theme_minimal_hgrid(font_family = "Roboto Condensed",
                                 rel_tiny = 10/14) +
    cowplot::panel_border() +
    scale_fill_manual(values = c("#81A7A6", "#5F799C", "#374685")) +
    theme(plot.background = element_rect(fill = "grey23"),
          axis.text = element_text(color = "grey85"),
          text = element_text(color = "white"))

ggsave(here::here("plots", "bike_commuting.png"))
