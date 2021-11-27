input <- readLines("2016/03-input")

valid_triangle <- function(x) {
  numbers <- stringr::str_extract_all(x, pattern = "[0-9]+")[[1]]
  numbers <- sort(as.numeric(numbers))
  numbers[3] < sum(numbers[-3])
}

sum(purrr::map_lgl(input, valid_triangle))
