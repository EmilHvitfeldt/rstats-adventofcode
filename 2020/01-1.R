input <- as.numeric(readLines("2020/01-input"))

sum2 <- function(input, target) {
  for (i in input) {
    for (j in input) {
      if (i + j == target) {
        return(i * j)
      }
    }
  }
}

sum2(input, 2020)
