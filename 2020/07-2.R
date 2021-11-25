library(stringr)
input <- readLines("2020/07-input")

bags <- str_extract_all(input, "(?<=[0-9] ).*?bag")
number <- str_extract_all(input, "[0-9]+")
number <- lapply(number, as.integer)
names(number) <- names(bags) <- str_extract(input, "^.+?bag")

find_size <- function(x, n) {
  contains <- bags[[x]]
  if (length(contains) == 0) return(n)
  n_bags <- map2_int(contains, number[[x]], find_size)
  sum(c(n_bags, 1L) * n)
}

find_size(x = "shiny gold bag", n = 1) - 1
