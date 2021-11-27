library(stringr)

input <- readLines("2018/03-input")

data <- str_match(input, "(?<id>[0-9]+) @ (?<left>[0-9]+),(?<top>[0-9]+): (?<width>[0-9]+)x(?<height>[0-9]+)")[,-1]
data <- apply(data, 2, as.numeric)

grid <- matrix(0, 1000, 1000)

for (i in seq_len(nrow(data))) {
  xs <- data[i, "left"] + seq_len(data[i, "width"])
  ys <- data[i, "top"] + seq_len(data[i, "height"])
  grid[xs, ys] <- grid[xs, ys] + 1
}

sum(grid >= 2)
