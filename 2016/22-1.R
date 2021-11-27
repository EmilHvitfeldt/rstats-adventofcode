library(tidyverse)
input <- readr::read_lines("2016/21-input", skip = 2) %>%
  strsplit(" +") %>%
  purrr::reduce(rbind) %>%
  tibble::as_tibble() %>%
  purrr::set_names(c("Filesystem", "Size", "Used", "Avail", "Use")) %>%
  tidyr::separate(Filesystem, c("tmp", "x", "y"), sep = "-") %>%
  mutate(across(x:y, ~ readr::parse_number(.x) %>% as.integer()))

library(dplyr)
pairs <- expand.grid(x0 = 0:29, y0 = 0:29, x1 = 0:29, y1 = 0:29) %>%
  filter((abs(x0 - x1) == 1) + (abs(y0 - y1) == 1) == 1)

pairs %>%
  left_join(input, by = c("x0" = "x", "y0" = "y")) %>%
  left_join(input, by = c("x1" = "x", "y1" = "y")) %>%
  filter(readr::parse_number(Used.x) <= readr::parse_number(Avail.y))
