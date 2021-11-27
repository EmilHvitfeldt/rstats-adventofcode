library(purrr)

input <- readLines("2017/04-input")

passwords <- strsplit(input, " ")

sum(map_lgl(passwords, ~ all(table(.x) == 1)))
