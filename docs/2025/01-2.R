input <- readLines("2025/01-input") |>
  stringr::str_replace_all(c("L" = "-", "R" = "")) |>
  as.integer()

zeros <- 0
position <- 50

for (value in input) {
  for (j in seq_len(abs(value))) {
    position <- position + sign(value)

    position <- position %% 100

    if (position == 0) {
      zeros <- zeros + 1
    }
  }
}

zeros
