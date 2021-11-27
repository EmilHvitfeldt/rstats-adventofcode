input <- readLines("2017/01-input")

chars <- as.numeric(strsplit(input, "")[[1]])

sum(chars[chars == c(chars[-1], chars[1])])
