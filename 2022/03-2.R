library(purrr)

input <- readLines("2022/03-input") |>
  strsplit(split = "")

input_group <- split(input, rep(seq_len(length(input) / 3), each = 3))

score <- c(letters, LETTERS)

common_group <- function(x) {
  x[[1]] |>
    intersect(x[[2]]) |>
    intersect(x[[3]])
}

map_chr(input_group, common_group) |>
  match(score) |>
  sum()
