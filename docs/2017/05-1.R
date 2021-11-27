input <- as.numeric(readLines("2017/05-input"))

len <- length(input)

index <- 1
steps <- 0

repeat {
  steps <- steps + 1
  offset <- input[index]
  input[index] <- input[index] + 1
  index <- index + offset

  if (index < 1 | index > len) break
}

steps
