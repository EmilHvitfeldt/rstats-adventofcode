input <- readLines("2025/03-input") |>
  stringr::str_split("") |>
  lapply(as.numeric)

f <- function(x) {
  repeat {
    if (length(x) == 12) {
      break
    }

    suppressWarnings(
      delete <- min(which(diff(x) > 0))
    )

    if (is.infinite(delete)) {
      x <- x[-length(x)]
      next
    }

    x <- x[-delete]
  }

  as.numeric(paste0(x, collapse = ""))
}

options(digits = 20)

lapply(input, f) |>
  unlist() |>
  sum()
