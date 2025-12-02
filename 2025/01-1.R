input <- readLines("2025/01-input") |>
  stringr::str_replace_all(c("L" = "-", "R" = "")) |>
  as.integer()

sum(cumsum(c(50, input)) %% 100 == 0)
