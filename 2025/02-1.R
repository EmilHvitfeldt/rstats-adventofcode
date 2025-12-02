library(stringr)

input <- readLines("2025/02-input")

input |>
  str_split(",") |>
  lapply(str_split, "-") |>
  unlist(recursive = FALSE) |>
  lapply(\(x) seq(x[1], x[2])) |>
  unlist() |>
  as.character() |>
  str_subset("^(\\d+)(\\1)$") |>
  as.numeric() |>
  sum()
