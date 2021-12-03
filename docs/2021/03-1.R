
input <- readLines("2021/03-input") |>
  strsplit("") |>
  purrr::reduce(rbind)

common_finder <- function(x, fun) {
  names(which(fun(table(x)) == table(x)))
}

rate_calculator <- function(mat, fun) {
  apply(mat, MARGIN = 2, FUN = common_finder, fun) |>
    paste0(collapse = "") |>
    strtoi(base = 2)
}

gamma <- rate_calculator(input, max)

epsilon <- rate_calculator(input, min)

gamma * epsilon
