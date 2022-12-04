library(purrr)

input <- readLines("2022/04-input") |>
  strsplit("[-,]") |>
  map(as.integer)

overlap <- function(x) {
  seq1 <- seq(x[1], x[2])
  seq2 <- seq(x[3], x[4])

  length(intersect(seq1, seq2)) > 0
}

map_lgl(input, overlap) |> sum()
