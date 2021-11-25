library(purrr)

input <- readLines("2020/03-input")

# turn input into logical matrix
mat <- strsplit(input, "") %>%
  map(~.x == "#") %>%
  reduce(rbind)

traverse <- function(mat, right, down) {
  x <- y <- 1

  height <- nrow(mat)
  width <- ncol(mat)

  # Check tree collision
  trees <- 0
  repeat {
    y <- y + down
    x <- (x + right) %% width
    x <- ifelse(x == 0, width, x)
    trees <- trees + mat[y, x]
    if (y >= height) break
  }
  trees
}

traverse(mat, 3, 1)
