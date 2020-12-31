
## Setup

library(tidyverse)

x <- read.csv(here::here("Advent of Code 2020/inputs/day1.csv"), header = FALSE) %>%
  as_vector()

## Part 1

df <- combn(x, 2) %>%
  t() %>%
  as_tibble() %>%
  mutate(total = V1 + V2) %>%
  filter(total == 2020) %>%
  mutate(mult = V1 * V2)

answer <- pull(df, mult)

answer

## Part 2

df_three <- combn(x, 3) %>%
  t() %>%
  as_tibble() %>%
  mutate(total = V1 + V2 + V3) %>%
  filter(total == 2020) %>%
  mutate(mult = V1 * V2 * V3)

answer_three <- pull(df_three, mult)

answer_three
