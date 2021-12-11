input <- readLines("2021/11-input") |>
  strsplit("") |>
  map(as.integer) |>
  reduce(rbind)

size <- nrow(input)

around <- function(x) {
  row <- x[1]
  col <- x[2]
  row_id <- c(row - 1, row - 1, row - 1, row, row + 1, row + 1, row + 1, row)
  col_id <- c(col - 1, col, col + 1, col + 1, col + 1, col, col - 1, col - 1)
  subset <- !(row_id > size | col_id > size)
  cbind(row_id[subset], col_id[subset])
}

flashes <- 0

for (i in 1:100) {
  flashed <- matrix(FALSE, nrow = size, ncol = size)
  input <- input + 1

  repeat {
    new_flashes <- which((input * !flashed) > 9, arr.ind = TRUE)

    if (nrow(new_flashes) == 0) break

    flashed <- flashed | (input > 9)

    bursts <- map(seq_len(nrow(new_flashes)), ~around(new_flashes[.x, ])) |>
      purrr::reduce(rbind)

    for (i in seq_len(nrow(bursts))) {
      input[bursts[i, 1], bursts[i, 2]] <- input[bursts[i, 1], bursts[i, 2]] + 1
    }
  }
  input[flashed] <- 0
  flashes <- flashes + sum(flashed)
}

flashes
