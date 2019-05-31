library(tidyverse)
library(ggridges)
library(ggthemes)

wine_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-28/winemag-data-130k-v2.csv")

wine_color <- function(variety) {
     case_when(
          variety %in% c("Riesling", "Pinot Gris", "Sauvignon Blanc",
                         "White Blend", "Sparkling Blend", "Portuguese White",
                         "Pinot Grigio", "Chardonnay") ~ "White",
          variety %in% c("Zinfandel", "Syrah", "Red Blend", "Portuguese Red",
                         "Bordeaux-style Red Blend", "Tempranillo", "Pinot Noir",
                         "Merlot", "Malbec", "Cabernet Sauvignon") ~ "Red",
          variety == "Rosé" ~ "Rosé"
     )
}

# best $20 wines by variety
p1 <- wine_ratings %>% 
     filter(price <= 20) %>%
     mutate(Color = wine_color(variety)) %>%
     add_count(variety) %>% 
     filter(n >= 700) %>% #remove wines with low numbers of reviews for less-cluttered graph
     ggplot(aes(points, fct_reorder(variety, points))) +
     stat_density_ridges(aes(fill = Color), quantile_lines = TRUE, quantiles = 2) +
     scale_fill_manual(values = c("firebrick", "rosybrown2", "lightyellow")) +
     xlim(81, 92) +
     labs(title = "Best Wines under $20",
          fill = "",
          caption = "\nSource: Kaggle
     Visualization @Frau_Dr_Barber") +
     theme_wsj(color = "gray") +
     theme(plot.title = element_text(hjust = 1),
           plot.caption = element_text(size = 10))

ggsave("wine.png", p1, dpi = "retina", height = 6, width = 5.5, units = "in")
