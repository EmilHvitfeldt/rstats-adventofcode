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

n_neighbors <- function(x, y, moves) {
  rowSums(matrix(outer(x, moves, `+`) %in% y, ncol = length(moves)))
}
black <- as.complex(names(table(flips)[table(flips) %% 2 == 1]))

for (i in 1:100) {
  stay_black <- n_neighbors(black, black, moves) == 1

  white_candidates <- map(black, ~.x + moves) %>%
    unlist() %>%
    unique()

  new_black_ind <- n_neighbors(white_candidates, black, moves) == 2

  new_black <- white_candidates[new_black_ind]

  black <- c(black[stay_black], new_black)
}

length(black)
