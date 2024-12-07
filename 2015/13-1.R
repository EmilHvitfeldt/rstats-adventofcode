input <- readLines("2015/13-input")

library(dplyr)
library(stringr)

from <- str_extract(input, "\\w*")
to <- str_extract(input, "\\w*\\.$")
to <- str_remove(to, "\\.")
happiness <- as.numeric(str_extract(input, "[0-9]+"))
negative <- str_detect(input, "lose")

input_df <- data.frame(
  from,
  to,
  happiness = happiness * ifelse(negative, -1, 1)
)

all_perms <- combinat::permn(unique(from), m = length(unique(from)))

table_happiness <- function(x) {
  seq_x <- seq_along(x)
  len_x <- length(x)

  data.frame(
    from = c(x, x),
    to = c(x[c(seq_x[-1], 1)], x[c(len_x, seq_x[-len_x])])
  ) %>%
    left_join(input_df, by = c("from", "to")) %>%
    summarize(sum = sum(happiness, na.rm = TRUE)) %>%
    pull(sum)
}

perm_values <- vapply(all_perms, table_happiness, numeric(1))
max(perm_values)
