input <- readLines("2016/02-input")

directions <- strsplit(input, "")

pad <- matrix(c(1, 4, 7, 2, 5, 8, 3, 6, 9), nrow = 3)

move <- list(U = c(-1, 0), D = c(1, 0), L = c(0, -1), R = c(0, 1))

pos <- c(2, 2)

password <- numeric()

for (i in directions) {
  for (j in i) {
    pos <- pmax(pmin(pos + move[[j]], 3), 1)
  }
  password <- c(password, pad[pos[1], pos[2]])
}

paste(password, collapse = "")
