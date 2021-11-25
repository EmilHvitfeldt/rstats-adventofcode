library(stringr)
library(purrr)

input <- readLines("2020/24-input")

moves <- c(
  e = 1 + 0i,
  se = 0 - 1i,
  sw = -1 - 1i,
  w = -1 + 0i,
  nw = 0 + 1i,
  ne = 1 + 1i
)
flips <- input %>%
  str_extract_all("[ns]?[ew]") %>%
  map(~moves[.x]) %>%
  map(sum) %>%
  reduce(c)

flips %>%
  table() %>%
  {sum(. %% 2 == 1)}
