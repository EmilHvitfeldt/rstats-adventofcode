read_list <- function(path, sep = "", type = identity) {
  lines <- readLines(path)
  res <- strsplit(lines, sep)
  res <- lapply(res, type)
  res
}

input <- read_list("2024/02-input", " ", as.integer)

safe <- function(x) {
  diffs <- diff(x)
  cond1 <- all(diffs > 0) || all(diffs < 0)
  cond2 <- all(abs(diffs) >= 1) && all(abs(diffs) <= 3)
  cond1 && cond2
}

vapply(input, safe, logical(1)) |>
  sum()
