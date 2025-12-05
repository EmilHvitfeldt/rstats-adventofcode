read_matrix <- function(path,
                        sep = "",
                        fill = NA,
                        type = identity) {
  lines <- readLines(path)
  tokens <- strsplit(lines, sep)
  token_lengths <- lengths(tokens)
  res <- matrix(fill,
                nrow = length(lines),
                ncol = max(token_lengths))

  for (i in seq_along(lines)) {
    res[i, seq_len(token_lengths[i])] <- type(tokens[[i]])
  }
  res
}

input <- read_matrix("2025/04-input")

before <- sum(input == "@")

around <- function(x,
                   y,
                   x_max = nrow(input),
                   y_max = ncol(input)) {
  xs <- x + c(-1, 0, 1)
  ys <- y + c(-1, 0, 1)

  xs <- xs[xs > 0]
  ys <- ys[ys > 0]
  xs <- xs[xs <= x_max]
  ys <- ys[ys <= y_max]

  input[xs, ys]
}

matches <- input
matches[] <- 0

repeat {
  for (row in seq_len(nrow(input))) {
    for (col in seq_len(ncol(input))) {
      matches[row, col] <- sum(around(row, col) == "@") - (input[row, col] == "@")
    }
  }

  delete <- matches < 4 & input == "@"

  if (!any(delete)) {
    break
  }

  input[delete] <- "."
}

after <- sum(input == "@")

before - after
