library(tidyverse)
library(tidytext)

ramen_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-04/ramen_ratings.csv")

ramen_ratings %>%
     group_by(brand) %>%
     summarize(n = n(),
               rating = mean(stars, na.rm = TRUE)) %>%
     filter(n >= 10) %>% 
     arrange(desc(rating))

ramen_ratings_tweaked <- ramen_ratings %>%
     mutate(brand = fct_lump(brand, 9),
            country = fct_lump(country, 9))

ramen_model <- lm(stars ~ country+brand, data = ramen_ratings_tweaked)

ramen_model %>% 
     tidy(conf.int = TRUE) %>% 
     arrange(desc(p.value))
     mutate(term = str_replace(term, "country", "Country: "),
            # term = str_replace(term, "style", "Style: "),
            # term = str_replace(term, "brand", "Brand: "),
            term = fct_reorder(term, estimate)) %>% 
     ggplot(aes(estimate, term)) +
     geom_vline(lty = 2, xintercept = 0) +
     geom_point() +
     geom_errorbarh(aes(xmin = conf.low, xmax = conf.high))
     
ramen_words <- ramen_ratings %>%
     unnest_tokens(word, variety) %>% 
     anti_join(stop_words, by = "word")
     #%>% filter(!word %in% c("noodles", "noodle", "ramen"))

top_ramen_words <- ramen_words %>%
     count(word, sort = TRUE) %>%
     head(125)

library(widyr)
library(ggraph)
library(igraph)
ramen_words_correlation <- ramen_words %>%
     filter(word %in% top_ramen_words$word) %>% 
     pairwise_cor(word, review_number, sort = TRUE) %>% 
     filter(correlation > 0.15)

ramen_word_pairs <- ramen_words %>% 
     filter(word %in% top_ramen_words$word) %>% 
     pairwise_count(word, review_number, sort = TRUE)

ramen_word_pairs %>%
     filter(n > 40) %>% 
     graph_from_data_frame() %>% 
     ggraph(layout = "fr") +
     geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
     geom_node_point(size = 5) +
     geom_node_text(aes(label = name), repel = TRUE, 
                    point.padding = unit(0.2, "lines")) +
     theme_void()

set.seed(5000)
# a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
# ramen_words_correlation %>% 
#      graph_from_data_frame() %>% 
#      ggraph(layout = "fr") + 
#      geom_edge_link(aes(edge_alpha = correlation), arrow = a, 
#                     end_cap = circle(.07, 'inches'), show.legend = FALSE) +
#      geom_node_point(color = "lightblue", size = 6) +
#      geom_node_text(aes(label = name), repel = TRUE) +
#      theme_void()

ramen_words_correlation %>% 
     graph_from_data_frame() %>% 
     ggraph(layout = "fr") + 
     geom_edge_link(aes(edge_alpha = correlation),  show.legend = FALSE) +
     geom_node_point(color = "lightblue", size = 6) +
     geom_node_text(aes(label = name), repel = TRUE) +
     labs(title = "Word correlations in Ramen Varities",
          subtitle = "using the 125 most common words for ramen varieties",
          caption = "Source: TheRamenRater.com
          Visualization @Frau_Dr_Barber") +
     theme_void() +
     theme(plot.title = element_text(hjust = 0.5),
           plot.subtitle = element_text(hjust = 0.5))

ramen_word_averages <- ramen_words %>% 
     group_by(word) %>%
     summarise(n = n(),
               mean_stars = mean(stars, na.rm = TRUE)) %>% 
     arrange(desc(n))

graph <- ramen_words_correlation %>% 
     rename(weight = correlation) %>%
     mutate(alpha = cut(weight, c(0.25, 0.5, 1))) %>% 
     graph_from_data_frame(vertices = ramen_word_averages)

ggraph(graph, layout = 'fr', niter = 15000) +
     geom_edge_link(aes(edge_alpha = alpha), edge_width = 0.2) +
     geom_node_point(aes(size = n)) +
     geom_node_text(
          aes(label = name), size = 3, repel = TRUE
     )
