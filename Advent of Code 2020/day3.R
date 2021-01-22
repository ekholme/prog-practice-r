
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

#writing a function to figure out which position we need to check
get_pos <- function(x, mult) {
  (x-1)*mult + 1
}

#realizing I need to extend my extended_vec even more
longer_vec <- map_chr(use_vec, ~paste(rep(.x, 73), collapse = ""))

#creating a df to figure out the positions to check against for each case
pos_df <- map_dfc(c(1, 3, 5, 7), ~get_pos(x = 1:length(extended_vec), mult = .x)) %>%
  slice(-1) #removing the top row bc it's our starting position

names(pos_df) <- c("pos1", "pos3", "pos5", "pos7")

#this gets our answers for all except the 'right 1 down 2' scenario
trees_df <- map_dfc(pos_df,
           ~map2_lgl(
             longer_vec[2:323],
             .,
             is_tree
           ))


#essentially the logic here is to get the odd numbered indices from the longer_vec
#for the 'down 2' scenario
keeps <- seq(1, 323, by = 2)
twos_vec <- longer_vec[keeps]

#now i can just repeat the process from earlier
pos_twos <- get_pos(x = 1:length(twos_vec), mult = 1)

twos_tmp <- map2_lgl(twos_vec[2:length(twos_vec)],
                     pos_twos[2:length(pos_twos)],
                     is_tree)

twos_sum <- sum(twos_tmp)

#getting final answer
part_two_answer <- trees_df %>%
  summarize(across(everything(), sum)) %>%
  t() %>%
  as.vector() %>%
  append(twos_sum) %>%
  reduce(`*`, .init = 1)
