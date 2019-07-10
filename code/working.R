library(tidyverse)
library(paletteer)

wwc_outcomes <- read_csv("D:/wwc_outcomes.csv")
# 
# wwc_outcomes %>%
#   unite("round_win_status", "round", "win_status") %>% 
#   mutate(
#     place = case_when(
#       round_win_status == "Final_Won" ~ 1,
#       round_win_status == "Final_Lost" ~ 2,
#       round_win_status == "Third Place Playoff_Won" ~ 3,
#       round_win_status == "Third Place Playoff_Lost" ~ 4,
#       round_win_status == "Quarter Final_Lost" ~ 5),
#     #placing = factor(placing, levels = c("1", "2", "3", "4", "5-8"))
#   ) %>% 
#   filter(!is.na(place)) %>%
#   mutate(team = fct_reorder2(team, desc(year), place)) %>% 
#   ggplot(aes(year, place, group = team)) +
#   geom_line(aes(color = team)) +
#   geom_point(aes(color = team)) +
#   scale_y_reverse() +
#   theme_classic()

wwc_outcomes %>%
  mutate(
    place = case_when(
    round == "Final" & win_status == "Won" ~ 1,
    round == "Final" & win_status == "Lost" ~ 2,
    round == "Third Place Playoff" & win_status == "Won" ~ 3,
    round == "Third Place Playoff"& win_status == "Lost" ~ 4,
    round == "Quarter Final" & win_status == "Lost" ~ 5
    )
  ) %>%
  filter(!is.na(place)) %>%
  #arrange(desc(year), place) %>% 
  mutate(highlight = if_else(team == "USA", "yes", "no")) %>% 
  ggplot(aes(year, place, group = team)) +
  geom_line(aes(color = highlight), size = 2) +
  geom_point(aes(color = highlight), size = 4) +
  geom_point(color = "white", size = 1) +
  labs(x = "World Cup",
       y = "",
       title = "The US women's team is the winningest team in world cup history",
       subtitle = "Despite this, they make dramatically less than male players - even though
they win more games and generate more revenue"
       ) +
  #geom_text(data = . %>% filter(year == 1991, place < 5), aes(label = team, x = 1989)) +
  geom_text(data = . %>% filter(year == 2019, place < 5), aes(label = team, x = 2021)) +
  scale_x_continuous(breaks = c(1991, 1995, 1999, 2003, 2007, 2011, 2015, 2019),
                     expand = c(.05, .05)) +
  scale_y_continuous(trans = "reverse", 
                     labels = c("1st", "2nd", "3rd", "4th",
                                "5th-8th\n(Quarter Finals)")) +
  scale_color_manual(values = c("gray", "red")) +
  #coord_cartesian(ylim = c(1:5))
  theme_minimal() +
  theme(legend.position = "none")


palettes_d_names %>% 
  as_tibble() %>% 
  filter(type == "qualitative") %>% 
  arrange(desc(length))

wwc_outcomes %>% 
  group_by(year, round) %>% 
  count() %>%
  arrange(year, n) %>% 
  view()
