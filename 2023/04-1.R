input <- readLines("2023/04-input")

input

winning <- input |>
  stringr::str_extract("\\:.*\\|") |>
  stringr::str_extract_all("\\d+") |>
  lapply(as.integer)

numbers <- input |>
  stringr::str_extract("\\|.*") |>
  stringr::str_extract_all("\\d+") |>
  lapply(as.integer)

score <- function(x) {
  if (x == 0) {
    return(0)
  }

  2 ^ (x - 1)
}

purrr::map2_int(winning, numbers, ~ score(sum(.x %in% .y))) |>
  sum()
