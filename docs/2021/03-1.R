
input <- readLines("2021/03-input") |>
  strsplit("") |>
  purrr::reduce(rbind)

common_finder <- function(x, fun, even) {
  tab <- table(x)
  if (length(tab) == 2 & tab[1] == tab[2]) return(even)
  names(which(fun(tab) == tab))
}

rate_calculator <- function(mat, fun, even) {
  apply(mat, MARGIN = 2, FUN = common_finder, fun, even) |>
    paste0(collapse = "") |>
    strtoi(base = 2)
}

gamma <- rate_calculator(input, max, "1")

epsilon <- rate_calculator(input, min , "0")

gamma * epsilon
