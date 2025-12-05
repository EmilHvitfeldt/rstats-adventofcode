input <- readLines("2025/05-input")

divide <- which(input == "")

fresh <- input[seq(1, divide - 1)]
aval <- input[seq(divide + 1, length(input))] |> as.numeric()

ranges <- fresh |>
  strsplit("-") |>
  lapply(as.numeric)

checker <- function(x) {
  fresh <- FALSE
  for (range in ranges) {
    if (x >= range[1] && x <= range[2]) {
      fresh <- TRUE
      break
    }
  }
  fresh
}

vapply(aval, checker, FUN.VALUE = logical(1)) |>
  sum()
