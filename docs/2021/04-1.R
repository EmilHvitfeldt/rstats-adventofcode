input <- readLines("2021/04-input")

read_matrix <- function(lines, sep = "", type = identity) {
  lines <- stringr::str_trim(lines)
  tokens <- strsplit(lines, sep)
  token_lengths <- lengths(tokens)
  res <- matrix(nrow = length(lines), ncol = max(token_lengths))

  for (i in seq_along(lines)) {
    res[i, seq_len(token_lengths[i])] <- type(tokens[[i]])
  }
  res
}

numbers <- strsplit(input[1], ",")[[1]] |> as.integer()

boards <- purrr::map(
  0:99,
  ~ read_matrix(input[3:7 + 6 * .x], "\\s+", type = as.integer)
)

check_board <- function(board) {
  for (i in seq_along(numbers)) {
    matched <- matrix(board %in% numbers[seq_len(i)], nrow = 5)

    row_checks <- apply(matched, MARGIN = 1, prod)
    col_checks <- apply(matched, MARGIN = 2, prod)
    if (any(c(row_checks, col_checks) == 1)) break
  }
  i
}

win_times <- purrr::map_int(boards, check_board)

fastest_time <- min(win_times)
fastest_board <- boards[[which(win_times == fastest_time)]]

sum(setdiff(fastest_board, numbers[seq_len(fastest_time)])) *
  numbers[fastest_time]
