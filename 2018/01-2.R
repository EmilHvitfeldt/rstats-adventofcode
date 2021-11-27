input <- readLines("2018/01-input")

input <- as.integer(input)
input_len <- length(input)

res <- list()

total <- 0

i <- 0
repeat {
  total <- total + input[i %% input_len + 1]
  if (is.null(res[as.character(total)][[1]])) {
    res[as.character(total)] <- 1
  } else {
    break
  }
  i <- i + 1
}
total
