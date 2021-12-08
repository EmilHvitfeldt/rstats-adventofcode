library(tidyverse)

input <- readLines("2021/08-input")

splitter <- function(x) {
  str_split(x, " ") %>%
    map(str_split, "") %>%
    map(map, sort)
}

setdiff_length <- function(x, y) {
  lengths(map(x, ~setdiff(x[[which(y)]], .x)))
}

minus1 <- function(x) x - 1

solver <- function(lights, right) {
  x1 <- lengths(lights) == 2
  x4 <- lengths(lights) == 4
  x7 <- lengths(lights) == 3
  x8 <- lengths(lights) == 7
  x6 <- lengths(lights) == 6 & setdiff_length(lights, x1) == 1
  x0 <- lengths(lights) == 6 & setdiff_length(lights, x4) == 1 & !x6
  x9 <- lengths(lights) == 6 & !x6 & !x0
  x5 <- lengths(lights) == 5 & setdiff_length(lights, x6) == 1
  x3 <- lengths(lights) == 5 & setdiff_length(lights, x9) == 1 & !x5
  x2 <- lengths(lights) == 5 & !x5 & !x3

  cont <- list(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9) %>%
    map(~lights[[which(.x)]]) %>%
    map(sort)

  right %>%
    match(cont) %>%
    minus1() %>%
    paste(collapse = "") %>%
    as.numeric()
}

tibble(input) %>%
  separate(input, c("left", "right"), sep = " \\| ") %>%
  mutate(across(c("left", "right"), splitter)) %>%
  mutate(res = map2_dbl(left, right, solver)) %>%
  summarise(total = sum(res))
