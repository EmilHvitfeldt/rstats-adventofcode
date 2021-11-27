library(purrr)

input <- readLines("2017/04-input")

passwords <- strsplit(input, " ")

order_letters <- function(x) {
  paste0(sort(strsplit(x, "")[[1]]), collapse = "")
}

sum(map_lgl(passwords, ~ all(table(map_chr(.x, order_letters)) == 1)))

