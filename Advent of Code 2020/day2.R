
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

df %>%
  mutate(check_min = if_else(str_sub(pw, min_cnt, min_cnt) == letter, TRUE, FALSE),
         check_max = if_else(str_sub(pw, max_cnt, max_cnt) == letter, TRUE, FALSE),
         num_checks = if_else(check_min + check_max == 1, TRUE, FALSE)) %>%
  summarize(answer = sum(num_checks))
