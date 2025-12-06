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

input <- read_matrix("2025/06-input", sep = "")
input[is.na(input)] <- " "

values <- input[-nrow(input), ]
operators <- input[nrow(input), ]

divs <- which(operators != " ") - 1
divs <- c(divs, ncol(values) + 1)

blocks <- list()
for (i in seq_len(length(divs) - 1)) {
  blocks[[i]] <- values[, seq(divs[i] + 1, divs[i + 1] - 1)]
}

parse_block <- function(x) {
  as.numeric(apply(x, 2, paste0, collapse = ""))
}

operators <- operators[operators != " "]
values <- lapply(blocks, parse_block)

options(digits = 20)

purrr::map2(values, operators, \(x, y) paste0(x, collapse = y)) |>
  lapply(\(x) eval(parse(text = x))) |>
  unlist() |>
  sum()
