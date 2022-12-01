input <- readLines("2022/01-input")
input <- as.numeric(input)

split(input, cumsum(is.na(input))) |>
  vapply(sum, na.rm = TRUE, FUN.VALUE = numeric(1)) |>
  max()
