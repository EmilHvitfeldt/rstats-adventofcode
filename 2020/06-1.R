library(tidyverse)
library(tidytext)

answers <- read_file("2020/06-input") %>%
  tibble(input = .) %>%
  unnest_paragraphs(text, input) %>%
  rowid_to_column("group_id") %>%
  unnest_tokens(text, text) %>%
  rowid_to_column("person_id") %>%
  unnest_characters(text, text)

answers %>%
  count(group_id, text) %>%
  nrow()
