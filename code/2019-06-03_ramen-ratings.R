library(tidyverse)
library(tidytext)
library(widyr)
library(igraph)
library(ggraph)

ramen_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-04/ramen_ratings.csv")

ramen_words <- ramen_ratings %>%
     unnest_tokens(word, variety) %>% 
     anti_join(stop_words, by = "word")

top_ramen_words <- ramen_words %>%
     count(word, sort = TRUE) %>%
     head(125)

ramen_words_correlation <- ramen_words %>%
     filter(word %in% top_ramen_words$word) %>% 
     pairwise_cor(word, review_number, sort = TRUE)

set.seed(13)
ramen_words_correlation %>%
     filter(correlation > 0.13) %>% 
     graph_from_data_frame() %>% 
     ggraph(layout = "fr") + 
     geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
     geom_node_point(color = "#C3272B", size = 3) +
     geom_node_text(aes(label = name), repel = TRUE) +
     labs(title = "Co-occuring Words in Ramen Flavors",
          subtitle = "among the 125 most common words\n",
          caption = "\nSource: TheRamenRater.com
          Visualization @Frau_Dr_Barber") +
     set_graph_style(family = "Century Schoolbook") +
     theme_void()+ 
     theme(text = element_text(family = "Century Schoolbook"),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          plot.margin = margin(0.25, 0.25, 0.25, 0.25, "in")) 

ggsave("ramen.png", dpi = 300, width = 7, height = 5, units = "in")
