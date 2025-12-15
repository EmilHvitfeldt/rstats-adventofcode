library(stringr)
input <- readLines("2017/09-input")

remove_garbage <- function(input) {
  garbage <- logical(length(input))

  i <- 1
  start <- NA
  end <- NA

  while (i <= length(input)) {
    cur <- input[i]
    if (cur == "!") {
      i <- i + 2
      next
    }
    if (cur == "<" && is.na(start)) {
      start <- i
    }
    if (cur == ">" && !is.na(start)) {
      end <- i

      garbage[seq(start, end)] <- TRUE
      start <- NA
      end <- NA
    }
    i <- i + 1
  }

  input[!garbage]
}

input <- input |>
  str_split_1("") |>
  remove_garbage()

input <- input[input != ","]

res <- 0
level <- 0

for (i in input) {
  if (i == "{") {
    level <- level + 1
  } else if (i == "}") {
    res <- res + level
    level <- level - 1
  }
}

res
