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

around <- function(row, col) {
  list(
    list(row = row + 0, col = col + 1),
    list(row = row + 1, col = col + 0),
    list(row = row + 0, col = col - 1),
    list(row = row - 1, col = col + 0)
  )
}

range_checker <- function(can) {
  !((can$row == 0) |
      (can$row > row_length) |
      (can$col == 0) |
      (can$col > col_length))
}

value_checker <- function(can, ref) {
  value <- input[can$row, can$col]
  last_value <- input[ref$row, ref$col]

  if (value == 9) return(FALSE)
  value > last_value
}

basin_size <- function(x) {

  candidates <- list(
    list(row = x[1], col = x[2])
  )

  saved <- list()

  repeat {
    new_candidaes <- around(candidates[[1]]$row, candidates[[1]]$col)
    new_candidaes <- setdiff(new_candidaes, saved)
    new_candidaes <- setdiff(new_candidaes, candidates)
    new_candidaes <- new_candidaes[map_lgl(new_candidaes, range_checker)]

    new_candidaes <- new_candidaes[
      map_lgl(new_candidaes, value_checker, candidates[[1]])
    ]

    candidates <- c(candidates, new_candidaes)
    saved <- c(saved, candidates[1])
    candidates[1] <- NULL

    if (length(candidates) == 0) break
  }
  length(saved)
}

largest_basins <- which(mat, arr.ind = TRUE) |>
  apply(1, basin_size)

sort(largest_basins, decreasing = TRUE)[1:3] |>
  prod()
