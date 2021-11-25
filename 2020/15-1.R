input <- c(16,1,0,18,12,14,19)

res <- rep(-1, 2020)
res[seq_along(input)] <- input
for (i in 8:2020) {
  if (sum(res == res[i- 1]) == 1) {
    res[i] <- 0
  } else {
    last_calls <- which(res == res[i- 1])
    res[i] <- last_calls[length(last_calls)] - last_calls[length(last_calls) - 1]
  }
}
rev(res)[1]
