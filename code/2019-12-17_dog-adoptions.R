library(tidyverse)
library(tidytext)
library(paletteer)

# get raw data
dog_descriptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_descriptions.csv')

# unnest tokens, remove stop words
tidy_dog_descriptions <- dog_descriptions %>%
    filter(!is.na(description)) %>% 
    unnest_tokens(word, description) %>% 
    mutate(word = str_extract(word, "[a-z]+")) %>% 
    anti_join(stop_words)

plot_df <- tidy_dog_descriptions %>%
    count(age, word, sort = TRUE) %>%
    group_by(age) %>% 
    mutate(prop = n / sum(n)) %>% 
    select(-n) %>%
    pivot_wider(names_from = age, values_from = prop)

#plot extras
palette <- paletteer::paletteer_d("wesanderson::IsleofDogs1")
baby_fill <- palette[[1]]
senior_fill <- palette[[2]]
line_col <- palette[[3]]
bg <- palette[[6]]
    
polygon_df <- tibble(
    x = c(0, Inf, Inf, 0, 0, 0, Inf, 0),
    y = c(0, 0, Inf, 0, 0, Inf, Inf, 0),
    poly = c(rep("baby", 4), rep("senior", 4)),
    fill = c(rep(baby_fill, 4), rep(senior_fill, 4))
)

annotations <- tibble(
    x = c(0.00006, 0.02),
    y = c(0.01, 0.0001),
    label = c("Words more common in\ndescribing senior dogs",
              "Words more\ncommon in\ndescribing\npuppies"),
    hjust = c(0, 1),
    vjust = c(0.5, 0.5)
)

#plotting time
plot_df %>% 
    ggplot(aes(Baby, Senior)) +
    geom_polygon(data = polygon_df,
                 aes(x, y, fill = fill), alpha = 0.5) +
    geom_abline(lty = 2, color = line_col) +
    geom_text(aes(label = word), check_overlap = TRUE, hjust = 0.5,
              family = "Futura Medium Italic") +
    scale_fill_identity() +
    scale_color_identity() +
    scale_x_log10(labels = scales::percent, limits = c(0.00005, 0.02)) +
    scale_y_log10(labels = scales::percent, limits = c(0.00005, 0.02)) +
    geom_text(data = annotations, family = "Futura Medium", size = 6,
              lineheight = 0.8,
              aes(x, y, label = label, hjust = hjust, vjust = vjust)) +
    labs(
        x = "Word frequency in puppy descriptions",
        y = "Word frequency in senior dog descriptions",
        title = "Word frequencies in adoption descriptions\nof puppies vs. senior dogs",
        caption = "Source: Petfinder.com\nViz @frau_dr_barber"
        ) +
    cowplot::theme_minimal_grid(font_family = "Futura Medium",
                                rel_large = 18/14, colour = NA) +
    theme(plot.background = element_rect(fill = bg),
          plot.title.position = "plot",
          plot.margin = margin(1, 2, 1, 1, "lines"))

ggsave(here::here("plots", "dog_adoptions.png"), height = 6.7, width = 6.7)

