input <- readLines("2016/03-input")

numbers <- unlist(stringr::str_extract_all(input, "[0-9]+"))

numbers <- matrix(as.numeric(t(matrix(as.numeric(numbers),
                                      byrow = FALSE, nrow = 3))),
                  byrow = TRUE, ncol = 3)

valid_triangle <- function(x) {
  x <- sort(as.numeric(x))
  x[3] < sum(x[-3])
}

sum(apply(numbers, 1, valid_triangle))
