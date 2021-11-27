library(ggplot2)

input <- readLines("2016/08-input")

rect <- function(grid, x, y) {
  grid[seq_len(x), seq_len(y)] <- 11
  grid
}

rotate <- function(x, by) {
  len <- length(x)
  x[c(seq(len - by + 1, len), seq(1, len -by))]
}

rotate_column <- function(grid, column, by) {
  grid[, column] <- rotate(grid[, column], by)
  grid
}

rotate_row <- function(grid, column, by) {
  grid[column, ] <- rotate(grid[column, ], by)
  grid
}

grid <- matrix(FALSE, nrow = 6, ncol = 50)

for (i in seq_along(input)) {
  action <- stringr::str_extract(input[i], "(rect|row|column)")
  dims <- as.numeric(stringr::str_extract_all(input[i], "[0-9]+")[[1]])

  if (action == "rect") {
    grid <- rect(grid, dims[2], dims[1])
  } else if (action == "row") {
    grid <- rotate_row(grid, dims[1]+1, dims[2])
  } else if (action == "column") {
    grid <- rotate_column(grid, dims[1]+1, dims[2])
  }
}

reshape2::melt(grid) %>%
  ggplot(aes(Var2, -Var1, fill = value)) +
  geom_tile() +
  coord_fixed() +
  theme_void() +
  guides(fill = "none")
