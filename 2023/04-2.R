input <- readLines("2023/04-input")

input

winning <- input |>
  stringr::str_extract("\\:.*\\|") |>
  stringr::str_extract_all("\\d+") |>
  lapply(as.integer)

numbers <- input |>
  stringr::str_extract("\\|.*") |>
  stringr::str_extract_all("\\d+") |>
  lapply(as.integer)

cards <- rep(1, length(input))

for (i in seq_along(cards)) {
  if (i == 0) next
  n <- cards[i]

  wins <- sum(winning[[i]] %in% numbers[[i]])

  cards[seq_len(wins) + i] <- cards[seq_len(wins) + i] + n
}

sum(cards)
