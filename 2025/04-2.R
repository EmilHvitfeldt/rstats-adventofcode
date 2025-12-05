input <- readLines("2025/04-input")
tokens <- strsplit(input, "")
token_lengths <- lengths(tokens)
mat <- matrix(nrow = length(input), ncol = max(token_lengths))

for (i in seq_along(input)) {
  mat[i, seq_len(token_lengths[i])] <- tokens[[i]]
}

before <- sum(mat == "@")

around <- function(x, y) {
  x_max <- nrow(mat)
  y_max <- ncol(mat)

  xs <- x + c(-1, 0, 1)
  ys <- y + c(-1, 0, 1)

  xs <- xs[xs > 0 & xs <= x_max]
  ys <- ys[ys > 0 & ys <= y_max]

  mat[xs, ys]
}

neighbors <- function(x, y) {
  sum(around(x, y) == "@") - (mat[x, y] == "@")
}

matches <- mat
matches[] <- 0

repeat {
  for (row in seq_len(nrow(mat))) {
    for (col in seq_len(ncol(mat))) {
      matches[row, col] <- neighbors(row, col)
    }
  }
  delete <- matches < 4 & mat == "@"

  if (!any(delete)) {
    break
  }

  mat[delete] <- "."
}

after <- sum(mat == "@")

before - after
