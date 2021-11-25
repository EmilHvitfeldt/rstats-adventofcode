input <- readLines("2020/11-input")

# turn input into logical matrix
mat <- strsplit(input, "") %>%
  reduce(rbind)

find_neighbor <- function(mat, i, j, x, y) {
  res <- "."
  i <- i + y
  j <- j + x
  while (i > 0 & j > 0 & j <= ncol(mat) & i <= nrow(mat)) {
    value <- mat[i,j]
    if (value != ".") {
      res <- value
      break
    }
    i <- i + y
    j <- j + x
  }
  res
}

find_all_neighbors <- function(mat, i, j) {

  up <-   find_neighbor(mat, i, j, 0, -1)
  down <- find_neighbor(mat, i, j, 0, 1)
  right <- find_neighbor(mat, i, j, -1, 0)
  left <-  find_neighbor(mat, i, j, 1, 0)

  upright <- find_neighbor(mat, i, j, 1, -1)
  upleft <- find_neighbor(mat, i, j, -1, -1)
  downright <- find_neighbor(mat, i, j, 1, 1)
  downleft <- find_neighbor(mat, i, j, -1, 1)

  c(up, down, right, left, upright, upleft, downright, downleft)
}

replace <- mat

repeat {
  for (i in seq_len(nrow(mat))) {
    for (j in seq_len(ncol(mat))) {
      if (mat[i, j] == ".") next

      conv <- find_all_neighbors(mat, i, j)


      if (mat[i, j] == "L") {
        if (all(conv != "#")) {
          replace[i, j] <- "#"
        }
      }

      if (mat[i, j] == "#") {
        if (sum(conv == "#") >= 5) {
          replace[i, j] <- "L"
        }
      }
    }
  }

  if (identical(mat, replace)) break

  mat <- replace
}

sum(mat == "#")
