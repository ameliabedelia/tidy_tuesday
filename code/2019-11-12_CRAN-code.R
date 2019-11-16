library(tidyverse)
library(ggtext)
library(ggridges)
library(glue)
library(cowplot)

# get data
cran_code <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-12/loc_cran_packages.csv")

# reshape df and calculate percent code, blank, and comment for each language
plot_df <- cran_code %>%
    mutate(total_code = blank + comment + code) %>% 
    mutate_at(vars(blank, comment, code), ~. / total_code) %>% 
    pivot_longer(cols = blank:code, 
                 names_to = "type", values_to = "frac") %>% 
    mutate( #focus on top 6 languages for plot
        type = fct_reorder(type, frac),
        language = fct_lump(language, 6),
        language = fct_infreq(language)
    ) %>%
    filter(language != "Other") 

# plot extras
labels = c(code = "<span style='color:#8be9fd'>**\\`code\\`**</span>", 
           blank = "<span style='color:#ffb86c'>\\<blank lines\\></span>", 
           comment = "<span style='color:#6272a4'>*\\# comment*</span>")

colors = c(c("#6272a4", "#ffb86c", "#8be9fd"))

# plot
plot_df %>% 
    ggplot(aes(frac, type)) +
    ggridges::geom_density_ridges(aes(fill = type, color = type), 
                                  size = 0.25, show.legend = FALSE) +
    facet_wrap(vars(language)) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_y_discrete(name = NULL, label = labels) +
    coord_cartesian(xlim = c(-0.05, 0.95)) +
    scale_fill_manual(values = colors) +
    scale_color_manual(values = colors) +
    labs(x = "", y = "",
         title = glue::glue("What's in a script? Breakdown of CRAN files 
                            into code, blank lines, and comments by language"),
         subtitle = "Among the top six languages used in R packages",
         caption = glue::glue("source data: CRAN via @philmassicotte
                              visualization: @frau_dr_barber")) +
    cowplot::theme_minimal_vgrid(font_family = "Source Code Pro", color = "#44475a",
                                 rel_tiny = 9/14) +
    cowplot::panel_border(color = "#44475a") +
    theme(
        axis.text.x = element_text(color = "#989cb3"),
        axis.text.y = element_markdown(),
        text = element_text(color = "#f8f8f2"),
        plot.background = element_rect(fill = "#282a36"),
        plot.title.position = "plot",
        panel.spacing = unit(1, "lines")
    ) 

ggsave(here::here("plots", "cran_code.png"))


