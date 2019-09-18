library(tidyverse)

# get data
park_visits <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/national_parks.csv")
centroids <- read_csv("~/Downloads/National_Park_Service__Park_Unit_Centroids.csv") %>% 
    rename_all(tolower) %>% #downloaded from https://public-nps.opendata.arcgis.com/datasets/national-park-service-park-unit-centroids
    mutate(label = str_extract(unit_name, "^.*(?=(National Park))"))
usa_df <- map_data("state")

# calculate average visits per year for last 10 years
ave_visits <- park_visits %>%
    mutate(year = parse_integer(year)) %>% 
    filter(year >= 2006, unit_type == "National Park") %>%
    group_by(unit_code) %>% 
    summarise(ave_visit = mean(visitors, na.rm = TRUE)) %>% 
    ungroup() 

# graph
centroids %>% 
    left_join(ave_visits) %>%
    filter(!state %in% c("AK", "HI")) %>% 
    ggplot(aes(x, y)) +
    geom_polygon(data = usa_df, aes(long, lat, group = group),
                 fill = "white", color = "#99542c", size = 0.25) +
    geom_point(aes(size = ave_visit), color = "#2d4b1e") +
    ggrepel::geom_text_repel(data = . %>% filter(!unit_code %in% c("GRSM", "DRTO", "VIIS")),
                             aes(label = label), family = "National Park", size = 2.1) +
    coord_map(xlim = c(-123, -69.5), ylim = c(25, 48.1)) +
    scale_size_area("Average annual visits", breaks = c(1e+7, 3e+7, 5e+7),
                    labels = c("10 million", "30 million", "50 million")) +
    annotate("text", x = -77, y = 30, size = 4, hjust = 0,
             family = "National Park", color = "white",
             label = glue::glue("Great Smoky
                        Mountains NP
                        sees the most
                        visitors annually")
    ) +
    annotate("segment", xend = -81.75, yend = 35.3, x = -75, y = 33.75,
             arrow = arrow(length = unit(0.03, "npc")),
             color = "#2d4b1e", size = 0.5) +
    labs(title = "The United States National Parks",
         subtitle = "Point size corresponds to average number of visitors (2006-2016)") +
    # National Park font downloaded from https://nationalparktypeface.com/
    cowplot::theme_map(font_size = 16, rel_tiny = 0.75,
                       font_family = "National Park") + 
    theme(text = element_text(color = "white"),
          legend.position = "bottom",
          legend.justification = "center",
          plot.background = element_rect(fill = "#99542c")
    ) 

ggsave("national_parks.png", width = 7, height = 5)
