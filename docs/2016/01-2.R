library(stringr)
input <- readLines("2016/01-input")

instructions <- strsplit(input, ", ")[[1]]

directions <- str_extract(instructions, ".{1}")
value <- as.numeric(str_extract(instructions, "[0-9]+"))

position <- matrix(0, nrow = 1, ncol = 2)
arrow <- matrix(c(0, 1), nrow = 1, ncol = 2)

last <- function(x) x[nrow(x), ]

for (i in seq_along(directions)) {
  if (directions[i] == "R") {
    arrow <- rev(arrow) * c(1, -1)
  } else if (directions[i] == "L") {
    arrow <- rev(arrow * c(1, -1))
  }

  for (step in seq_len(value[i])) {
    position <- rbind(position, arrow + last(position))
  }
}

sum(abs(position[which(duplicated(position))[1], ]))
