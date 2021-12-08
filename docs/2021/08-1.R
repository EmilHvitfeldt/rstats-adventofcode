library(tidyverse)

readLines("2021/08-input") %>%
  str_remove(".*\\| ") %>%
  str_split(" ") %>%
  map(nchar) %>%
  map_int(~length(.x[.x %in% c(2, 4, 3, 7)])) %>%
  sum()
