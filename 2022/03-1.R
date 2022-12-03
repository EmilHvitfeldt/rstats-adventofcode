library(purrr)

input <- readLines("2022/03-input") |>
  strsplit(split = "")

score <- c(letters, LETTERS)

common_split <- function(x) {
  len <- length(x) / 2
  intersect(x[seq_len(len)], x[seq_len(len) + len])
}

map_chr(input, common_split) |>
  match(score) |>
  sum()

input_group <- split(input, rep(seq_len(length(input) / 3), each = 3))

common_group <- function(x) {
  x[[1]] |>
   intersect(x[[2]]) |>
   intersect(x[[3]])
}

map_chr(input_group, common_group) |>
  match(score) |>
  sum()
