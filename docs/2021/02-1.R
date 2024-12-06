input <- read.delim("2021/02-input", sep = " ", header = FALSE)

horizontal <- 0
depth <- 0

for (i in seq_len(nrow(input))) {
  command <- input[i, 1]
  value <- input[i, 2]
  if (command == "forward") {
    horizontal <- horizontal + value
  }
  if (command == "up") {
    depth <- depth - value
  }
  if (command == "down") {
    depth <- depth + value
  }
}

depth * horizontal
