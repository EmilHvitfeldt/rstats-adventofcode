library(purrr)
library(stringr)

input <- readLines("2020/02-input")

password_check <- function(x) {
  numbers <- as.numeric(str_extract_all(x, "[0-9]+")[[1]])
  chars <- str_extract_all(x, "[a-z]+")[[1]]

  count <- str_count(chars[2], chars[1])
  (numbers[1] <= count) & (numbers[2] >= count)
}

sum(map_lgl(input, password_check))
