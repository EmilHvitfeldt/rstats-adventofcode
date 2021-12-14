library(tidyverse)

template <- readLines("2021/14-input")[1]
pairs <- readLines("2021/14-input")[-(1:2)]

from <- str_sub(pairs, 1, 2)

adj <- map(pairs, ~ c(paste0(str_sub(.x, 1, 1), str_sub(.x, -1, -1)),
                      paste0(str_sub(.x, -1, -1), str_sub(.x, 2, 2))))
names(adj) <- from

ref_counts <- counts <- set_names(integer(length(from)), from)
for (i in seq(1, nchar(template) - 1)) {
  pair <- str_sub(template, i, i + 1)
  counts[pair] <- counts[pair] + 1
}

for (step in 1:10) {
  new_counts <- ref_counts
  for (pair in names(new_counts)) {
    new_counts[adj[[pair]][1]] <- new_counts[adj[[pair]][1]] + counts[pair]
    new_counts[adj[[pair]][2]] <- new_counts[adj[[pair]][2]] + counts[pair]
  }
  counts <- new_counts
}

tibble(
  count = c(counts, 1),
  char = c(str_sub(names(counts), 1, 1), str_sub(template, -1, -1))
) %>%
  count(char, wt = count, sort = TRUE) %>%
  summarise(max(n) - min(n))
