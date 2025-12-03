input <- readLines("2025/03-input") |>
  stringr::str_split("") |>
  lapply(as.numeric)

x <- input[[1]]


f <- function(x) {
  repeat {
    if (length(x) == 2) {
      break
    }

    if (x[1] < x[2]) {
      x <- x[-1]
      next
    }

    if (x[2] > x[3]) {
      x <- x[-3]
      next
    } else {
      x <- x[-2]
      next
    }
  }

  sum(x * c(10, 1))
}


f(x = input[[2]])
lapply(input, f) |>
  unlist() |>
  sum()
