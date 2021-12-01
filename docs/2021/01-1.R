input <- readLines("2021/01-input") |>
  as.numeric()

sum(diff(input) > 0)
