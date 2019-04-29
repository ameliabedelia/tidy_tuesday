library(tidyverse)

bird_collisions <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-30/bird_collisions.csv")
mp_light <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-30/mp_light.csv")

big_birds <- bird_collisions %>% 
     filter(locality == "MP") %>% 
     left_join(mp_light) %>% 
     drop_na(light_score)

glimpse(big_birds)
