input <- readLines("2025/05-input")

divide <- which(input == "")

fresh <- input[seq(1, divide - 1)]

ranges <- fresh |>
  strsplit("-") |>
  lapply(as.numeric)

overlap <- function(x, y) {
  any(x[2] >= y) && any(x[1] <= y) || any(y[2] >= x) && any(y[1] <= x)
}
combine <- function(x, y) {
  c(min(x[1], y[1]), max(x[2], y[2]))
}

finished_ranges <- list()

done <- FALSE

while (!done) {
  repeat {
    matched_ranges <- c()

    if (length(ranges) == 1) {
      finished_ranges <- c(finished_ranges, ranges)
      done <- TRUE
      break
    }

    for (i in seq(2, length(ranges))) {
      if (overlap(ranges[[1]], ranges[[i]])) {
        matched_ranges <- c(matched_ranges, i)
      }
    }

    if (length(matched_ranges) == 0) {
      finished_ranges <- c(finished_ranges, ranges[1])
      ranges[1] <- NULL
      break
    }

    for (id in rev(matched_ranges)) {
      ranges[[1]] <- combine(ranges[[1]], ranges[[id]])
      ranges[[id]] <- NULL
    }

  }
}

options(digits = 20)

finished_ranges |>
  purrr::map_dbl(\(x) x[2] - x[1] + 1) |>
  sum()
