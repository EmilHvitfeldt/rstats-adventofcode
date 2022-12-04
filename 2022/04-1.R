library(purrr)

input <- readLines("2022/04-input") |>
  strsplit("[-,]") |>
  map(as.integer)

contain <- function(x) {
  seq1 <- seq(x[1], x[2])
  seq2 <- seq(x[3], x[4])

  length(setdiff(seq1, seq2)) == 0 ||
  length(setdiff(seq2, seq1)) == 0
}

map_lgl(input, contain) |> sum()
