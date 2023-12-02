input <- readLines("2023/02-input")

blue <- function(x) {
  x |>
    stringr::str_extract("\\d+ blue") |>
    stringr::str_extract("\\d++") |>
    as.numeric()
}
green <- function(x) {
  x |>
    stringr::str_extract("\\d+ green") |>
    stringr::str_extract("\\d++") |>
    as.numeric()
}
red <- function(x) {
  x |>
    stringr::str_extract("\\d+ red") |>
    stringr::str_extract("\\d++") |>
    as.numeric()
}

counts <- function(x) {
  tibble::tibble(blue = blue(x), green = green(x), red = red(x))
}

library(tidyverse)

input |>
  stringr::str_remove("Game \\d+: ") |>
  stringr::str_split("; ") |>
  setNames(seq_along(input)) |>
  purrr::map(counts) |>
  purrr::list_rbind(names_to = "id") |>
  summarize(
    .by = id,
    across(everything(), max, na.rm = TRUE)
  ) |>
  filter(red <= 12, green <= 13, blue <= 14) |>
  summarise(sum(as.numeric(id)))
