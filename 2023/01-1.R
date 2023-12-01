input <- readLines("2023/01-input")

input |>
  stringr::str_extract_all("\\d") |>
  purrr::map_chr(~ paste0(head(.x, 1), tail(.x, 1))) |>
  as.integer() |>
  sum()
