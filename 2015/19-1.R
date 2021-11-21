input <- readLines("2015/19-input")

molecules <- input[length(input)]

key <- input[-length(input)]
key <- key[key != ""]

library(dplyr)
library(tidyr)
library(stringr)

key_tbl <- tibble(key) %>%
  separate(key, c("from", "to"), " => ")

keys <- split(key_tbl$to, key_tbl$from)

res <- character()

for (k in seq_along(keys)) {
  k <- keys[k]

  locs <- str_locate_all(molecules, names(k))[[1]]

  for (i in seq_len(nrow(locs))) {
    new_mole <- molecules

    str_sub(new_mole, locs[i, "start"], locs[i, "end"]) <- k[[1]]

    res <- c(res, new_mole)
  }
}

length(unique(res))
