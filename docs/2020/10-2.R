input <- sort(as.integer(readLines("2020/10-input")))

runs <- rle(c(diff(c(0, input))))
weights <- c(1, 2, 4, 7)

options(scipen = 999)
prod(weights[runs$lengths[runs$values == 1]])
