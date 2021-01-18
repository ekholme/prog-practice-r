
#link to puzzle: https://adventofcode.com/2020/day/2


# Setup -------------------------------------------------------------------

library(tidyverse)

df_raw <- vroom::vroom(here::here("Advent of Code 2020/inputs/day2.csv"), col_names = FALSE) %>%
  set_names(c("range", "letter", "pw"))



# Cleaning Data -----------------------------------------------------------

df <- df_raw %>%
  mutate(letter = str_remove_all(letter, ":")) %>%
  extract(range, c("min_cnt", "max_cnt"), "(.*)-(.*)", remove = FALSE, convert = TRUE)


# Getting Answer ----------------------------------------------------------

df %>%
  mutate(letter_count = map2_int(pw, letter, str_count),
         valid = if_else(letter_count >= min_cnt & letter_count <= max_cnt, TRUE, FALSE)) %>%
  summarize(answer = sum(valid))



# Part 2 ------------------------------------------------------------------


