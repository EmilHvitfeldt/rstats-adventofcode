library(purrr)

input <- readLines("2021/09-input") |>
  strsplit("") |>
  map(as.integer) |>
  reduce(rbind)

row_length <- nrow(input)
col_length <- ncol(input)

mat <- matrix(FALSE, row_length, col_length)

for (row in 1:row_length) {
  for (col in 1:col_length) {
    row_id <- c(row + 1, row, row - 1, row)
    col_id <- c(col, col + 1, col, col - 1)

    subset <- !(row_id > row_length | col_id > col_length)

    row_id <- row_id[subset]
    col_id <- col_id[subset]

    if (all(input[cbind(row_id, col_id)] > input[row, col])) {
      mat[row, col] <- TRUE
    }
  }
}

sum(input[mat] + 1)
