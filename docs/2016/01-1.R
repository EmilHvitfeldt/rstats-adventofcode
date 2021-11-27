library(stringr)
input <- readLines("2016/01-input")

instructions <- strsplit(input, ", ")[[1]]

directions <- str_extract(instructions, ".{1}")
value <- as.numeric(str_extract(instructions, "[0-9]+"))

position <- c(0, 0)
arrow <- c(0, 1)

for (i in seq_along(directions)) {
  if (directions[i] == "R") {
    arrow <- rev(arrow) * c(1, -1)
  } else if (directions[i] == "L") {
    arrow <- rev(arrow * c(1, -1))
  }
  position <- position + arrow * value[i]
}
sum(abs(position))
