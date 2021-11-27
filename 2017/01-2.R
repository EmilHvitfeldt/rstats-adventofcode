input <- readLines("2017/01-input")

chars <- as.numeric(strsplit(input, "")[[1]])

len <- seq_len(length(chars)/2)

sum(chars[chars == c(chars[-len], chars[len])])
