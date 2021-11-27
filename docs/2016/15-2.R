input <- readLines("2016/15-input")

input <- input |> stringr::str_match_all("\\d+")

n_pos <- c(purrr::map_int(input, ~as.integer(.x[2, ])), 11)
pos <- c(purrr::map_int(input, ~as.integer(.x[4, ])), 0)

i <- 0
repeat {
  if (all((pos + seq_along(pos) + i) %% n_pos == 0)) break
  i <- i + 1
}
i
