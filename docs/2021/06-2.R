input <- scan("2021/06-input", sep = ",")

counts <- c(0, tabulate(input, nbins = 8))

for (i in seq_len(256)) {
  n0 <- counts[1]
  counts[-length(counts)] <- counts[-1]
  counts[7] <- counts[7] + n0
  counts[9] <- n0
}

options(scipen = 999)
sum(counts)
