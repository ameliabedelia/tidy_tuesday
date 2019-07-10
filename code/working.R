library(tidyverse)

wwc_outcomes <- read_csv("D:/wwc_outcomes.csv")

placing <- function(round, win_status) {
  case_when(
    round == "Final" & win_status == "Won" ~ "1",
    round == "Final" & win_status == "Lost" ~ "2",
    round =="Third Place Playoff" & win_status == "Won", ~ "3",
    round =="Third Place Playoff"& win_status == "Lost", ~ "4",
    round == "Quarter Fiinal"& win_status == "Lost" ~ "5-8"
  )
}

wwc_outcomes %>% 
  mutate(place = placing(round, win_status))

wwc_outcomes %>%
  unite("round_win_status", "round", "win_status") %>% 
  mutate(
    place = case_when(
      round_win_status == "Final_Won" ~ 1,
      round_win_status == "Final_Lost" ~ 2,
      round_win_status == "Third Place Playoff_Won" ~ 3,
      round_win_status == "Third Place Playoff_Lost" ~ 4,
      round_win_status == "Quarter Final_Lost" ~ 5),
    #placing = factor(placing, levels = c("1", "2", "3", "4", "5-8"))
  ) %>% 
  filter(!is.na(place)) %>%
  mutate(team = fct_reorder2(team, desc(year), place)) %>% 
  ggplot(aes(year, place, group = team)) +
  geom_line(aes(color = team)) +
  geom_point(aes(color = team)) +
  scale_y_reverse() +
  theme_classic()


  mutate(placing = case_when(
   isTRUE(round == "Final" && win_status == "Won") ~ "1")) %>% view()
   round == "Final" && win_status == "Lost" ~ "2",
   round =="Third Place Playoff" && win_status == "Won", ~ "3",
   round =="Third Place Playoff"&& win_status == "Lost", ~ "4",
   round == "Quarter Fiinal"&& win_status == "Lost" ~ "5-8"
  )) %>% 
  view()
