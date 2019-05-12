library(tidyverse)
library(paletteer)
library(ggthemes)
library(patchwork)

# Datasets ----
student_ratio <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-07/student_teacher_ratio.csv")

Europe_countries <- c("Albania", "Andorra", "Austria",  "Belarus",
                      "Belgium", "Bosnia and Herzegovina", "Bulgaria", 
                      "Croatia", "Cyprus", "Czech Republic", "Denmark", 
                      "Estonia","Finland", "France", "Germany", "Greece", 
                      "Hungary", "Ireland", "Italy", "Iceland", "Kosovo", 
                      "Latvia", "Liechtenstein","Lithuania", "Luxembourg", 
                      "Malta", "Moldova", "Montenegro", "Monaco", "Macedonia", 
                      "Netherlands", "Norway", "Poland", "Portugal", 
                      "Romania", "San Marino", "Serbia","Slovakia", "Slovenia",
                      "Spain", "Sweden", "Switzerland", "Ukraine", "UK")

map_data <- map_data("world") 
     
#fix country names to match names in Map database
student_ratio <- student_ratio %>% 
     mutate(
          country = str_replace_all(country, c("Czechia" = "Czech Republic",
                                               "United Kingdom of Great Britain and Northern Ireland" = "UK",
                                               "Russian Federation" = "Russia",
                                               "Republic of Moldova" = "Moldova",
                                               "The former Yugoslav Republic of Macedonia" = "Macedonia")),
          indicator = factor(indicator, levels = c("Pre-Primary Education",
                                                   "Primary Education",
                                                   "Lower Secondary Education",
                                                   "Upper Secondary Education",
                                                   "Secondary Education",
                                                   "Post-Secondary Non-Tertiary Education",
                                                   "Tertiary Education"
          ))
     )
# collapse educaiton levels to combine lower and upper secondary
student_ratio_collapse <- student_ratio %>% 
     mutate(indicator = fct_collapse(indicator, 
                                     "Secondary Education" = c("Lower Secondary Education", "Upper Secondary Education",
                                                               "Secondary Education"),
                                     "Post-secondary Education" = c("Post-Secondary Non-Tertiary Education",
                                                                    "Tertiary Education"))
     )

# Map of Primary Education Class Sizes ----
primary <- student_ratio_collapse %>% 
     filter(country %in% Europe_countries, indicator == "Primary Education",
            country != "Iceland") %>% #sorry Iceland
     group_by(country) %>% 
     summarise(
          n = n(),
          average = mean(student_ratio, na.rm = TRUE)
     ) 

plot_primary <- primary %>% 
     left_join(map_data, by = c("country" = "region")) %>% 
     ggplot(aes(x = long, y = lat)) +
     geom_polygon(aes(fill = average, group = group), color = "black", size = 0.2) +
     coord_map("mollweide", ylim = c(30, 72)) +
     theme_economist() +
     labs(x = "",
          y = "",
          fill = "Student-to-teacher ratio",
          title = "Primary Education\n(ISCED 1)") +
     scale_fill_paletteer_c("ggthemes", "Red-Green-White Diverging", -1,
                            breaks = c(6, 9, 12, 15, 18)) +
     theme(axis.line.x = element_blank(),
           axis.text = element_blank(),
           axis.ticks = element_blank(),
           panel.grid = element_blank(),
           legend.position = "bottom",
           legend.direction = "horizontal",
           plot.title = element_text(hjust = 0.5, size = rel(1.4)),
           plot.margin = unit(c(0.2,0.2,0.2,0.2),"cm")
     )

# Map of Secondary Education Class Sizes ---- 
secondary <- student_ratio_collapse %>% 
     filter(country %in% Europe_countries, indicator == "Secondary Education",
            country != "Iceland") %>% #sorry again Iceland
     group_by(country) %>% 
     summarise(
          n = n(),
          average = mean(student_ratio, na.rm = TRUE)
     ) 

plot_secondary <- secondary %>% 
     left_join(map_data, by = c("country" = "region")) %>% 
     ggplot(aes(x = long, y = lat)) +
     geom_polygon(aes(fill = average, group = group), color = "black", size = 0.2) +
     theme_economist() +
     coord_map("mollweide", ylim = c(30, 72)) +
     scale_fill_paletteer_c("ggthemes", "Red-Green-White Diverging", -1,
                            breaks = c(6, 9, 12, 15, 18)) +
     labs(x = "",
          y = "",
          fill = "Student-to-teacher ratio",
          title = "Secondary Education\n(ISCED 2 & 3)") +
     theme(axis.line.x = element_blank(),
           axis.text = element_blank(),
           axis.ticks = element_blank(),
           panel.grid = element_blank(),
           legend.position = "bottom",
           legend.direction = "horizontal",
           plot.title = element_text(hjust = 0.5, size = rel(1.4)),
           plot.margin = unit(c(0.2,0.2,0.2,0.2),"cm")
     )

# Primary vs. secondary comparison ----
primary_secondary_comparison <- primary %>% 
     inner_join(secondary, by = "country") %>% 
     rename(primary = average.x, secondary = average.y) %>% 
     select(country, primary, secondary) %>% 
     mutate(diff = secondary - primary,
            perc_diff = (secondary / primary) - 1
            )

top_5 <- primary_secondary_comparison %>% top_n(5, perc_diff)
bottom_5 <- primary_secondary_comparison %>% top_n(-5, perc_diff)

plot_comparison <- bind_rows(top_5, bottom_5) %>% 
     ggplot(aes(fct_reorder(country, perc_diff), perc_diff)) +
     geom_col() +
     scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                        position = "right") +
     coord_flip() +
     labs(x = "", 
          y = "Positive values = secondary school\nclasses larger than primary classes\n",
          title = "\nCountries with the greatest differences in\nprimary and secondary school class sizes\n\n"
     ) +
     theme_economist() +
     theme(panel.grid.major = element_line(size = 0.45),
           axis.text = element_text(size = rel(1.3)),
           axis.title = element_text(size = rel(1.3)),
           aspect.ratio = 1.4,
           plot.title = element_text(hjust = 0.7, size = rel(1.4))
     )

# Combine plots
grid <- plot_primary + plot_comparison + plot_secondary +
     plot_layout(ncol = 3, width = c(1, 0.6, 1)) +
     plot_annotation(theme = theme_economist(),
                     title = "Student-to-Teacher Ratios in European Primary and Secondary Schools (2012-2017)",
                     caption = "Source: UNESCO
                     Visualization: @Frau_Dr_Barber")

ggsave("teacher_ratios.png", dpi = "retina", height = 8, width = 15, units = "in")
