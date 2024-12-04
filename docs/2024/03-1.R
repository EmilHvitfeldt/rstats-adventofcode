input <- readLines("2024/03-input") |>
  paste0(collapse = "")

mul <- function(x, y) x * y

input |>
  stringr::str_extract_all("mul\\(\\d+,\\d+\\)") |>
  unlist() |>
  purrr::map_int(\(x) eval(parse(text = x))) |>
  sum()
