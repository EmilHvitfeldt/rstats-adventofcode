input <- "^.^^^.^..^....^^....^^^^.^^.^...^^.^.^^.^^.^^..^.^...^.^..^.^^.^..^.....^^^.^.^^^..^^...^^^...^...^."
rows <- 40

tiles <- strsplit(input, "")[[1]] == "^"

room <- matrix(FALSE, nrow = rows, ncol = length(tiles) + 2)

room[1, seq_along(tiles) + 1] <- tiles

for (row in seq(2, nrow(room))) {
  for (col in seq(2, ncol(room) - 1)) {
    parent <- room[row - 1, -1:1 + col]

    if (
      identical(c(T, T, F), parent) ||
      identical(c(F, T, T), parent) ||
      identical(c(T, F, F), parent) ||
      identical(c(F, F, T), parent)
    ) {
      room[row, col] <- TRUE
    }
  }
}

sum(!room[, seq_along(tiles) + 1])
