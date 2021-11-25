library(purrr)
library(stringr)

input <- readLines("2020/02-input")

password_check_2 <- function(x) {
  numbers <- as.numeric(str_extract_all(x, "[0-9]+")[[1]])
  chars <- str_extract_all(x, "[a-z]+")[[1]]

  loc1 <- str_sub(chars[2], numbers[1], numbers[1])
  loc2 <- str_sub(chars[2], numbers[2], numbers[2])

  sum(loc1 == chars[1], loc2 == chars[1]) == 1

}

sum(map_lgl(input, password_check_2))
