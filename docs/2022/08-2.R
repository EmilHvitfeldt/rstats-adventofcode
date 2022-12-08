library(tidyverse)

read_matrix <- function(path, sep = "", fill = NA, type = identity) {
  lines <- readLines(path)
  tokens <- strsplit(lines, sep)
  token_lengths <- lengths(tokens)
  res <- matrix(fill, nrow = length(lines), ncol = max(token_lengths))

  for (i in seq_along(lines)) {
    res[i, seq_len(token_lengths[i])] <- type(tokens[[i]])
  }
  res
}

input <- read_matrix("2022/08-input", type = as.integer)

res <- matrix(0, nrow = nrow(input), ncol = ncol(input))

n_row <- nrow(input)
n_col <- ncol(input)

distances <- function(input, rows, cols) {
  above <- input[rows, cols]
  suppressWarnings(distance <- min(which(!(tree > above))))
  if (distance == Inf) {
    distance <- length(above)
  }
  distance
}

for (row in seq_len(n_row)) {
  for (col in seq_len(n_col)) {
    tree <- input[row, col]
    total <- 1

    # Above
    if (row > 1) {
      distance <- distances(input, seq(row - 1, 1), col)
      total <- total * distance
    }

    # below
    if (row < n_row) {
      distance <- distances(input, seq(row + 1, n_row), col)
      total <- total * distance
    }

    # left
    if (col > 1) {
      distance <- distances(input, row, seq(col - 1, 1))
      total <- total * distance
    }

    # right
    if (col < n_col) {
      distance <- distances(input, row, seq(col + 1, n_col))
      total <- total * distance
    }
    res[row, col] <- total
  }
}

max(res)
