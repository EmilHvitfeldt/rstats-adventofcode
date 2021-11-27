input <- readLines("2016/02-input")

directions <- strsplit(input, "")

pad <- matrix(c(NA, NA, 5, NA, NA,
                NA, 2, 6, "A", NA,
                1, 3, 7, "B", "D",
                NA, 4, 8, "C", NA,
                NA, NA, 9, NA, NA), nrow = 5)

move <- list(U = c(-1, 0), D = c(1, 0), L = c(0, -1), R = c(0, 1))

pos <- c(3, 1)

password <- numeric()

for (i in directions) {
  for (j in i) {

    my_move <- move[[j]]
    if (j == "U" & pad[pos[1], pos[2]] %in% c("5", "2", "1", "4", "9") ||
        j == "D" & pad[pos[1], pos[2]] %in% c("5", "A", "D", "C", "9") ||
        j == "L" & pad[pos[1], pos[2]] %in% c("1", "2", "5", "A", "D") ||
        j == "R" & pad[pos[1], pos[2]] %in% c("1", "4", "9", "C", "D")) {
      my_move <- c(0, 0)
    }

    pos <- pos + my_move
  }
  password <- c(password, pad[pos[1], pos[2]])
}

paste(password, collapse = "")
