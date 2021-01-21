
#link to puzzle: https://adventofcode.com/2020/day/3

# Setup -------------------------------------------------------------------

library(tidyverse)

df_raw <- read_csv(here::here("Advent of Code 2020/inputs/day3.csv"), col_names = FALSE) %>%
  mutate(X1 = str_replace_all(X1, c("\\." = "0", "\\#" = "1"))) #replacing so I don't have to deal with regex later

#reformatting as vector
use_vec <- df_raw %>%
  pull()


# Extending Data ----------------------------------------------------------

#extending pattern in data
extended_vec <- map_chr(use_vec, ~paste(rep(.x, 32), collapse = ""))


# Finding Trees -----------------------------------------------------------

#get the position to test against
pos <- map_dbl(1:length(extended_vec), function(x) {(x-1)*3 + 1})

#function to test if we hit a tree
is_tree <- function(vec, pos) {
  str_sub(vec, pos, pos) == "1"
}

tmp <- map2_lgl(extended_vec[2:length(extended_vec)], 
         pos[2:length(pos)],
         is_tree)

answer <- sum(tmp)

answer

#things to keep in mind:
#1. the pattern will continue beyond what's currently shown (so will need to append to the entries)
#2. need to get a simple function to calculate the position in each string to check against

#row 2 will be the 4th position; row 3 will be the 7th; row 4 will be the 10th; row 5 will be the 13th
#so the function is (rownum-1)*3 + 1 -- that is the position to check against



# Part 2 ------------------------------------------------------------------


  