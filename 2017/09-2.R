library(stringr)
input <- readLines("2017/09-input")

input |>
  str_remove_all("!.") |>
  str_extract_all("<.*?>") |>
  lapply(\(x) nchar(x) - 2) |>
  unlist() |>
  sum()
