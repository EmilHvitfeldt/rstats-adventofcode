input <- as.numeric(readLines("2020/09-input"))

i <- 1

repeat {
  sums <- colSums(combn(input[seq(i, i + 24)], 2))
  if(!any(input[i + 25] == sums)) break
  i <- i + 1
}

target <- input[i + 25]

i <- 1
step <- 1

repeat {
  res <- sum(input[seq(i, i + step)])

  if (res == target) break

  if (res > target) {
    i <- i + 1
    step <- 1
    next
  }
  step <- step + 1
}

sum(range(input[seq(i, i + step)]))
