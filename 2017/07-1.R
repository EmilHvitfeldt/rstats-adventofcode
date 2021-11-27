input <- readLines("2017/07-input")

library(stringr)

loc <- "ymfjt"

repeat {

  next_id <- str_detect(input, paste0(".+", loc))

  if (all(!next_id)) break

  loc <- str_extract(input[next_id], "^[a-z]+")
}
loc
