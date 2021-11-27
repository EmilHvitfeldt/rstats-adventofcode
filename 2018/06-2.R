library(dplyr)
library(stringr)

input <- readLines("2018/06-input")

point <- str_match(input, "(?<x>[0-9]+), (?<y>[0-9]+)")[,-1]
point <- apply(point, 2, as.numeric)

make_range <- function(x) {
  r <- range(point[,"x"])
  seq(r[1]- 100, r[2] + 100)
}

grid <- expand.grid(X = make_range(point[,"x"]), Y = make_range(point[,"y"]))

classify <- function(x, y) {

  dists <- abs(point[, "x"] - x) + abs(point[, "y"] - y)

  which_min <- which(dists == min(dists))

  if (length(which_min) != 1) {
    return(0)
  } else {
    return(which_min)
  }
}

grid <- grid %>%
  mutate(class = purrr::map2_dbl(X, Y, classify))

classify2 <- function(x, y) {
  dists <- abs(point[, "x"] - x) + abs(point[, "y"] - y)

  sum(dists) < 10000
}

grid %>%
  mutate(class2 = purrr::map2_dbl(X, Y, classify2)) %>%
  filter(class2 == 1) %>%
  nrow()
