library(tidyverse)
library(ggalluvial)
library(paletteer)
library(cowplot)

# get dataset
nobel_winners <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winners.csv")

# change historic countries to their modern day equivalents
nobel_winners_fixed <- nobel_winners %>% 
     filter(!is.na(birth_country), !is.na(death_country)) %>% 
     #364 winners are still alive and excluded from this visualization
     mutate(birth_country_modern = str_extract(birth_country, "\\(([^()]*)\\)"),
            birth_country_modern = substr(birth_country_modern, 2, nchar(birth_country_modern)-1),
            death_country_modern = str_extract(death_country, "\\(([^()]*)\\)"),
            death_country_modern = substr(death_country_modern, 2, nchar(death_country_modern)-1),
            birth_country = if_else(str_detect(birth_country, "\\("), birth_country_modern, birth_country),
            death_country = if_else(str_detect(death_country, "\\("), death_country_modern, death_country)
            ) %>%
     select(-birth_country_modern, -death_country_modern) %>%
     # assign Northern Ireland and Scotland to UK and abbrevaite UK and USA
     mutate(birth_country = str_replace_all(birth_country,
                                            c("Northern Ireland" = "UK",
                                            "Scotland" = "UK",
                                            "United Kingdom" = "UK",
                                            "United States of America" = "USA")),
            death_country = str_replace_all(death_country,
                                            c("Northern Ireland" = "UK",
                                              "Scotland" = "UK",
                                              "United Kingdom" = "UK",
                                              "United States of America" = "USA"))
            ) %>%
     # condense countries to top 6 for less cluttered visualization
     mutate(birth_country = fct_lump(birth_country, 5),
            death_country = fct_lump(death_country, 5))

# time to plot
plot <- nobel_winners_fixed %>%
     filter(category %in% c("Medicine", "Chemistry", "Physics")) %>% 
     select(full_name, birth_country, death_country) %>%
     gather(birth_country, death_country, key = "Event", value = "Country") %>% 
     mutate(Event = str_replace_all(Event, c("birth_country" = "Place of Birth",
                                             "death_country" = "Place of Death")),
            Country = fct_infreq(Country)) %>%
     distinct() %>% 
     ggplot(aes(x = Event, stratum = Country, alluvium = full_name,
                fill = Country, label = Country, y = 1)) +
     geom_flow(alpha = 0.7) +
     geom_stratum(alpha = 0.8, size = 0) +
     geom_text(stat = "stratum", size = 4) +
     annotate("text", x = 1.75, y = -25, 
              label = "(Laureates still alive excluded from analysis)") +
     scale_x_discrete(expand = c(0.1, 0.1), position = "top") +
     scale_fill_paletteer_d(ggsci, light_uchicago) +
     labs(x = "", 
          y = "Number of Nobel Laureates",
          title = "Mobility Among Nobel Laureates in the Sciences",
          caption = "Source: Kaggle
          Visualization @Frau_Dr_Barber") +
     theme(
           plot.caption = element_text(size = 9),
           axis.line = element_blank(),
           axis.ticks = element_blank(),
           legend.position = "none",
           ) 

save_plot("nobel.png", plot, base_height = 4, base_width = 6)
