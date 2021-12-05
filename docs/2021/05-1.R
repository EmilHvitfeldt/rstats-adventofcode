library(tidyverse)

tibble(input = readLines("2021/05-input")) %>%
  separate(input, into = c("x1", "y1", "x2", "y2"), convert = TRUE) %>%
  filter(x1 == x2 | y1 == y2) %>%
  group_nest(row_number()) %>%
  mutate(crosses = map(data, ~tibble(x = .x$x1:.x$x2, y = .x$y1:.x$y2))) %>%
  unnest(crosses) %>%
  count(x, y) %>%
  filter(n > 1) %>%
  nrow()
