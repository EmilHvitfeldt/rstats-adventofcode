library(magrittr)
library(stringr)
library(dplyr)
library(tidyr)

input <- readLines("2015/16-input")

sues <- input %>%
  str_remove("^Sue [0-9]*: ")

things <- sues %>%
  str_remove_all(": [0-9]*") %>%
  str_split(", ") %>%
  unlist()

values <- sues %>%
  str_extract_all("[0-9]+") %>%
  unlist() %>%
  as.numeric()

tibble(sue = rep(seq_len(length(input)), each = 3),
       things, values) %>%
  pivot_wider(names_from = things,values_from = values) %>%
  filter(
    children == 3 | is.na(children),
    cats == 7 | is.na(cats),
    samoyeds == 2 | is.na(samoyeds),
    pomeranians == 3 | is.na(pomeranians),
    akitas == 0 | is.na(akitas),
    vizslas == 0 | is.na(vizslas),
    goldfish == 5 | is.na(goldfish),
    trees == 3 | is.na(trees),
    cars == 2 | is.na(cars),
    perfumes == 1 | is.na(perfumes)
  )
