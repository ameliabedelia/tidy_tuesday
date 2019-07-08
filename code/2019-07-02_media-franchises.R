library(tidyverse)
library(ggthemes)
library(patchwork)

# load and clean datasets ----
media_franchises <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-02/media_franchises.csv")

#film revenue data from https://www.boxofficemojo.com/
hp_movie_revenue <- read_csv2("hp_movie_revenue.csv") %>%
     janitor::clean_names() %>% 
     mutate_at(vars(contains("gross")), parse_number) %>% 
     mutate(release = lubridate::mdy(release))

#book revenue from https://en.wikipedia.org/wiki/List_of_best-selling_books
hp_book_revenue <- read_csv2("hp_book_revenue.csv") %>% 
     janitor::clean_names()

# graphing ----
house_colors <- c("#9c1203", "#FFC500", "#033807", "#00165e", "#000000", "#8A8F81")

background <- "#f8f2e4"

#overall revenue graph
overall <- media_franchises %>% 
     filter(str_detect(franchise, "Harry Potter")) %>% 
     distinct() %>% 
     mutate(revenue_category = fct_reorder(revenue_category, revenue)) %>% 
     ggplot(aes(revenue_category, revenue, fill = revenue_category)) +
     geom_col(show.legend = FALSE) +
     coord_flip() +
     scale_fill_manual(values = rev(house_colors)) +
     labs(x = "",
          y = "Revenue\n(in billions)",
          title = "Overall",
          caption = "Source: Wikipedia") +
     theme_classic() +
     theme(text = element_text(family = "Garamond"),
           line = element_blank(),
           panel.background = element_rect(fill = background),
           panel.grid.major.x = element_line(color = "black", linetype = "dotted"),
           axis.title.x = element_text(family = "AbleNew", size = rel(1.5)),
           axis.text.y = element_text(size = rel(1.7)),
           plot.title = element_text(family = "AbleNew", size = rel(2))
          )

#film revenue graph
film <- hp_movie_revenue %>%
     arrange(release) %>% 
     mutate(title = str_c(c(1:6, "7a", "7b", ".", "."), ". ", title),
            title = fct_reorder(title, release),
            adjusted_gross = adjusted_gross / 10^6) %>% 
     ggplot(aes(fct_rev(title), adjusted_gross)) +
     geom_col(fill = house_colors[1], show.legend = FALSE) + 
     coord_flip() +
     labs(x = "",
          y = "Adjusted gross revenue\n(in millions)",
          title = "Film Revenue",
          caption = "Source: Box Office Mojo") +
     theme_classic() +
     theme(text = element_text(family = "Garamond"),
           line = element_blank(),
           panel.background = element_rect(fill = background),
           panel.grid.major.x = element_line(color = "black", linetype = "dotted"),
           axis.title.x = element_text(family = "AbleNew", size = rel(1.5)),
           axis.text.y = element_text(size = rel(1.5)),
           plot.title = element_text(family = "AbleNew", size = rel(2))
     )

#book revenue graph
book <- hp_book_revenue %>% 
     mutate(book = str_c(1:7, ". ", book)) %>% 
     ggplot(aes(fct_rev(book), approximate_sales_million)) +
     geom_col(fill = house_colors[3], show.legend = FALSE) +
     coord_flip() +
     labs(x = "",
          y = "Approximate sales\n(in millions)",
          title = "Books Sales",
          caption = "Source: Wikipedia") +
     theme_classic() +
     theme(text = element_text(family = "Garamond"),
           line = element_blank(),
           panel.background = element_rect(fill = background),
           panel.grid.major.x = element_line(color = "black", linetype = "dotted"),
           axis.title.x = element_text(family = "AbleNew", size = rel(1.5)),
           axis.text.y = element_text(size = rel(1.5)),
           plot.title = element_text(family = "AbleNew", size = rel(2)),
           plot.margin = margin(0, 10, 0, 0, "lines")
     )

#title & caption
title <- ggplot(data.frame(x = 1, y = 1:10)) +
     labs(x = NULL, y = NULL,
          title = "The Magical World of Harry Potter Revenue",
          subtitle = "The franchise has grossed an estimated 35 billion\nRevenue comes primarily from the box office, books, and merch\n"
          ) +
     theme(line = element_blank(),
           rect = element_rect(fill = "transparent"),
           plot.title = element_text(family = "Harry P", size = rel(6)),
           plot.subtitle = element_text(family = "AbleNew", size = rel(2.1)),
           panel.background = element_rect(fill = "transparent"),
           plot.background = element_rect(fill = "transparent", color = "transparent"),
           panel.border = element_rect(color = "transparent"),
           axis.text = element_blank())

caption <- ggplot(data.frame(x = 1, y = 1:10)) +
     labs(x = NULL, y = NULL,
          caption = "Visualization by Frau_Dr_Barber\n(Slytherin House)") +
     theme(line = element_blank(),
           rect = element_rect(fill = "transparent"),
           plot.caption = element_text(family = "AbleNew", size = rel(1.2)),
           panel.background = element_rect(fill = "transparent"),
           plot.background = element_rect(fill = "transparent", color = "transparent"),
           panel.border = element_rect(color = "transparent"),
           axis.text = element_blank())

#combine plots using patchwork
title + 
     (overall + film + book + plot_layout(widths = c(0.95, 0.75, 0.75))) + 
     caption +
     plot_layout(nrow = 3, heights = c(0, 20, 0)) +
     plot_annotation(theme = theme_wsj())

ggsave("harry_potter.png", height = 7.5, width = 18, units = "in")
