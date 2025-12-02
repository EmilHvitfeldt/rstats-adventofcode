input <- readLines("2025/02-input")
library(tidyverse)

Ids <- input |>
  str_split(",") |>
  lapply(str_split, "-") |>
  unlist(recursive = FALSE) |>
  lapply(\(x) seq(x[1], x[2])) |>
  unlist()

is_invalid <- function(x) {
  len <- nchar(x)
  if (len %% 2 == 1) {
    return(FALSE)
  }
  left <- str_sub(x, 1, len/2)
  right <- str_sub(x, len/2+1, len)

  left == right
}

Ids |>
  purrr::keep(is_invalid) |>
  sum()
