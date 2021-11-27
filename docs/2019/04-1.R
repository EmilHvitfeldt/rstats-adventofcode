library(purrr)

input <- 136818:685979

chars <- input |>
  as.character() |>
  strsplit("") |>
  map(as.numeric)

map_lgl(chars, ~ any(diff(.x) == 0) & all(diff(.x) >= 0)) |>
  sum()
