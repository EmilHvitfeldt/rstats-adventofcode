input <- scan("2017/06-input")

seen <- paste(input, collapse = " ")

input_len <- length(input)

cycles <- 0

repeat {
  cycles <- cycles + 1

  index <- min(which(max(input) == input))
  blocks <- input[index]
  input[index] <- 0

  while (blocks > 0) {
    index <- index + 1
    if (index > input_len) {
      index <- 1
    }
    input[index] <- input[index] + 1
    blocks <- blocks - 1
  }

  if (paste(input, collapse = " ") %in% seen) break
  seen <- c(seen, paste(input, collapse = " "))
}

length(seen) - which(paste(input, collapse = " ") == seen) + 1
