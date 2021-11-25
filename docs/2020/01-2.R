input <- as.numeric(readLines("2020/01-input"))

sum3 <- function(input, target) {
  for (i in input) {
    for (j in input) {
      for (l in input) {
        if (i + j + l == target) {
          return(i * j * l)
        }
      }
    }
  }
}

sum3(input, 2020)
