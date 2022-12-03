library(purrr)

input <- readLines("2022/03-input") |>
  strsplit(split = "")

score <- c(letters, LETTERS)

common_split <- function(x) {
  half <- length(x) / 2
  intersect(
    head(x, half), tail(x, half)
  )
}

map_chr(input, common_split) |>
  match(score) |>
  sum()
