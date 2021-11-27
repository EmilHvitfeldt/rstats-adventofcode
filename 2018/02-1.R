input <- readLines("2018/02-input")

chars <- strsplit(input, "")
x <- input[[1]]

count_2 <- function(x) {
  any(table(x) == 2)
}

count_3 <- function(x) {
  any(table(x) == 3)
}

sum(purrr::map_lgl(chars, count_2)) * sum(purrr::map_lgl(chars, count_3))
