value <- 20151125

x <- 1
y <- 1

repeat {
  if (y == 1) {
    y <- x + 1
    x <- 1
  } else {
    y <- y - 1
    x <- x + 1
  }
  value <- (value * 252533) %% 33554393
  if (x == 3075 & y == 2981) break
}

value
