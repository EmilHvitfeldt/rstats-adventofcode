f <- function(x) {
  repeat {
    if (length(x) == 2) {
      break
    }

    if (x[1] < x[2]) {
      x <- x[-1]
    } else if (x[2] > x[3]) {
      x <- x[-3]
    } else {
      x <- x[-2]
    }
  }

  sum(x * c(10, 1))
}

readLines("2025/03-input") |>
  strsplit("") |>
  lapply(as.numeric) |>
  lapply(f) |>
  unlist() |>
  sum()
