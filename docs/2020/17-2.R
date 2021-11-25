library(dplyr)
library(tidyr)
input <- readLines("2020/17-input")


start <- tibble(
  x = rep(seq(7, 14), 8),
  y = rev(rep(seq(7, 14), each = 8)),
  z = 0,
  w = 0,
  state = strsplit(input, "") %>% unlist()
)

space <- expand_grid(x = seq(1, 20),
                     y = seq(1, 20),
                     z = seq(-6, 6),
                     w = seq(-6, 6)) %>%
  left_join(start, by = c("x", "y", "z", "w")) %>%
  mutate(state = if_else(is.na(state), ".", state)) %>%
  mutate(row = row_number())

find_neighbors <- function(dat) {
  space %>%
    filter(abs(x - dat$x) <= 1,
           abs(y - dat$y) <= 1,
           abs(z - dat$z) <= 1,
           abs(w - dat$w) <= 1,
           !(x == dat$x & y == dat$y & z == dat$z & w == dat$w)) %>%
    pull(row)
}

all_neighbors <- map(seq_len(nrow(space)), ~find_neighbors(space[.x, ]))

next_state <- function(x, nbs) {
  if (x == "#") {
    if (sum(nbs == "#") %in% c(2, 3)) {
      return("#")
    } else {
      return(".")
    }
  } else {
    if (sum(nbs == "#") == 3) {
      return("#")
    } else {
      return(".")
    }
  }
}

for(iter in 1:6) {
  all_neighbors_states <- map(all_neighbors, ~ space$state[.x])
  space$state <- map2_chr(space$state, all_neighbors_states, next_state)
}

sum(space$state == "#")
