f <- function(x) {
  repeat {
    if (length(x) == 12) {
      break
    }

    diff <- diff(x)

    if (all(diff <= 0)) {
      x <- head(x, -1)
      next
    }

    x <- x[-min(which(diff > 0))]
  }

  as.numeric(paste0(x, collapse = ""))
}

options(digits = 20)

readLines("2025/03-input") |>
  strsplit("") |>
  lapply(as.numeric) |>
  lapply(f) |>
  unlist() |>
  sum()
