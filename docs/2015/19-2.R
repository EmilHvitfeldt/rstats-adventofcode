library(dplyr)
library(tidyr)
library(stringr)

input <- readLines("2015/19-input")

molecules <- input[length(input)]

key <- input[-length(input)]
key <- key[key != ""]

key_tbl <- tibble(key) %>%
  separate(key, c("from", "to"), " => ")

rev_keys <- split(key_tbl$from, key_tbl$to)
count <- 0

repeat {
  if (molecules == "e") break

  r_key <- sample(rev_keys, 1)


  if (str_detect(molecules, names(r_key))) {
    count <- count + str_count(molecules, names(r_key))
    molecules <- str_replace_all(molecules, names(r_key), r_key[[1]])
  }

}

count
