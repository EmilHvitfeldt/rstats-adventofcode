read_matrix <- function(path, sep = "") {
  lines <- readLines(path)
  lines <- trimws(lines)
  tokens <- strsplit(lines, sep)
  token_lengths <- lengths(tokens)
  res <- matrix(nrow = length(lines), ncol = max(token_lengths))

  for (i in seq_along(lines)) {
    res[i, seq_len(token_lengths[i])] <- tokens[[i]]
  }
  res
}

input <- read_matrix("2025/06-input", sep = " +")

values <- input[-nrow(input), ]
operators <- input[nrow(input), ]

values <- apply(values, 2, as.numeric, simplify = FALSE)

options(digits = 20)

purrr::map2(values, operators, \(x, y) paste0(x, collapse = y)) |>
  lapply(\(x) eval(parse(text = x))) |>
  unlist() |>
  sum()
