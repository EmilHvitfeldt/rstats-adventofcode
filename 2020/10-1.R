input <- as.integer(readLines("2020/10-input"))

res <- table(diff(sort(input)))

prod(res + 1)
