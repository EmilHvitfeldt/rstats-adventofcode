input <- as.numeric(readLines("2020/09-input"))

i <- 1

repeat {
  sums <- colSums(combn(input[seq(i, i + 24)], 2))
  if(!any(input[i + 25] == sums)) break
  i <- i + 1
}

input[i + 25]
