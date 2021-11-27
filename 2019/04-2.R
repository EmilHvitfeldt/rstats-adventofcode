library(purrr)

input <- 136818:685979

chars <- input |>
  as.character() |>
  strsplit("") |>
  map(as.numeric)

check <- function(x) {
  x_rle <- rle(diff(x))
  if (all(x_rle$values != 0)) {
    return(FALSE)
  }

  if (any(x_rle$lengths[x_rle$values == 0] == 1)) {
    return(TRUE)
  }
  FALSE
}

map_lgl(chars, ~ all(diff(.x) >= 0) & check(.x)) |>
  sum()
