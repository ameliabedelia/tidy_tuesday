library(tidyverse)
library(tidytext)
library(widyr)
library(igraph)
library(ggraph)
library(emojifont)

ramen_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-04/ramen_ratings.csv")

ramen_ratings <- read_csv("D:/ramen_ratings.csv")

ramen_words <- ramen_ratings %>%
     unnest_tokens(word, variety)

top_ramen_words <- ramen_words %>%
     count(word, sort = TRUE) %>%
     head(125)

ramen_words_correlation <- ramen_words %>%
     filter(word %in% top_ramen_words$word) %>% 
     pairwise_cor(word, review_number, sort = TRUE)


set.seed(1)
ramen_words_correlation %>%
     filter(correlation > 0.12) %>% 
     graph_from_data_frame() %>% 
     ggraph(layout = "fr") + 
     geom_edge_link(aes(edge_alpha = correlation),  color = "#654321", show.legend = FALSE) +
     #geom_node_point(color = "red", size = 5) +
     #geom_node_point(color = "seashell", size = 3) +
     geom_node_text(aes(label = emoji("ramen"))) +
     geom_node_text(aes(label = name), repel = TRUE) +
     labs(title = "Mapping Ramen Noodle Word Correlations",
          subtitle = "using the 125 most common words for ramen varieties\n",
          caption = "Source: TheRamenRater.com
          Visualization @Frau_Dr_Barber") +
     theme_void()+ 
     theme(#plot.background = element_rect(fill = "seashell"),
           plot.title = element_text(hjust = 0.5),
           plot.subtitle = element_text(hjust = 0.5),
           plot.margin = margin(0.25, 0.25, 0.25, 0.25, "in"))


# incorporate rating and freq ----

ramen_word_averages <- ramen_words %>%
     filter(word %in% top_ramen_words$word) %>%
     group_by(word) %>%
     summarize(
          total = n(),
          avg_stars = mean(stars, na.rm = TRUE)
     ) %>%
     rename(name = word) %>%
     arrange(desc(total))

graph <- ramen_words_correlation %>%
     filter(correlation >= 0.25)
     rename(weight = correlation) %>%
     mutate(alpha = cut(weight, c(0.25, 0.5, 1))) %>%
     graph_from_data_frame(vertices = ramen_word_averages)

ggraph(graph, layout = 'fr', niter = 5) +
     geom_edge_link(aes(edge_alpha = alpha), edge_width = 0.2) +
     geom_node_point(aes(size = total, color = avg_stars)) +
     geom_node_text(
          aes(label = name), size = 3, repel = TRUE
     )
