library(stringr)
input <- readLines("2017/09-input")
input <- str_split_1(input, "")

garbage <- logical(length(input))

inside <- FALSE
i <- 1

while (i <= length(input)) {
  garbage[i] <- inside

  cur <- input[i]

  if (cur == "<") {
    inside <- TRUE
  } else if (cur == ">") {
    inside <- FALSE
    garbage[i] <- FALSE
  } else if (cur == "!") {
    garbage[i] <- FALSE
    garbage[i + 1] <- FALSE
    i <- i + 1
  }
  i <- i + 1
}

sum(garbage)
