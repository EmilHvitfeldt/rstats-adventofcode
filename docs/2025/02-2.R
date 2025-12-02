input <- readLines("2025/02-input")
library(tidyverse)

Ids <- input |>
  str_split(",") |>
  lapply(str_split, "-") |>
  unlist(recursive = FALSE) |>
  lapply(\(x) seq(x[1], x[2])) |>
  unlist() |>
  as.character()

is_invalid <- function(x) {
  x <- str_split_1(x, "")
  len <- length(x)

  if (len == 1) {
    return(FALSE)
  }

  for (val in seq(1, len/2)) {
    if (round(len/val) == len/val) {
      group <- rep(seq_len(len/val), each = val)
      if (length(unique(split(x, group))) == 1) {
        return(TRUE)
      }
    }
  }

  FALSE
}

Ids |>
  purrr::keep(is_invalid) |>
  as.numeric() |>
  sum()
