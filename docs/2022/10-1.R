input <- readLines("2022/10-input")

X <- 1
cycles <- 0

for (i in input) {
  if (i == "noop") {
    cycles <- c(cycles, X)

  } else {
    cycles <- c(cycles, X)
    cycles <- c(cycles, X)
    val <- as.integer(substr(i, 6, nchar(i)))
    X <- X + val
  }
}

cycles <- cycles[-1]

index <- c(20, 60, 100, 140, 180, 220)

sum(cycles[index] * index)
