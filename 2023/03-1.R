read_matrix <- function(path, sep = "", fill = NA, type = identity) {
  lines <- readLines(path)
  tokens <- strsplit(lines, sep)
  token_lengths <- lengths(tokens)
  res <- matrix(fill, nrow = length(lines), ncol = max(token_lengths))

  for (i in seq_along(lines)) {
    res[i, seq_len(token_lengths[i])] <- type(tokens[[i]])
  }
  res
}

mat <- read_matrix("2023/03-input")

partnumber <- matrix(FALSE, nrow = nrow(mat), ncol = ncol(mat))

numbers <- as.character(0:9)

invalids <- c(numbers, ".")

around <- function(x, y, x_max = nrow(mat), y_max = ncol(mat)) {
  xs <- x + c(-1, 0, 1)
  ys <- y + c(-1, 0, 1)

  xs <- xs[xs > 0]
  ys <- ys[ys > 0]
  xs <- xs[xs <= x_max]
  ys <- ys[ys <= y_max]


  mat[xs, ys]
}

for (i in seq_len(nrow(mat))) {
  for (j in seq_len(ncol(mat))) {
    partnumber[i, j] <- ifelse(
      mat[i, j] %in% numbers,
      any(!around(i, j) %in% invalids),
      FALSE
    )
  }
}

gears <- reshape2::melt(mat == "*") |>
  dplyr::filter(value)

input <- readLines("2023/03-input")

number_locs <- input |>
  stringr::str_locate_all("\\d+")

partnumbers <- c()

for (i in seq_along(input)) {
  rows <- number_locs[[i]]
  for (row in seq_len(nrow(rows))) {
    start <- rows[row, 1]
    end <- rows[row, 2]
    if (any(partnumber[i, seq(start, end)])) {
      partnumbers <- c(partnumbers, stringr::str_sub(input[i], start, end))
    }
  }
}

partnumbers |>
  as.numeric() |>
  sum()


