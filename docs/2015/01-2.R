input <- readLines("2015/01-input")
chars <- strsplit(input, "")[[1]]

key <- c("(" = 1, ")" = -1)

min(which(cumsum(key[chars]) < 0))
