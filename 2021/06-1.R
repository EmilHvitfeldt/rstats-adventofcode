input <- scan("2021/06-input", sep = ",")

for (i in seq_len(80)) {
  input <- input - 1
  if (any(input < 0)) {
    input <- c(input, rep(8, sum(input < 0)))
    input[input < 0] <- 6
  }
}

length(input)
