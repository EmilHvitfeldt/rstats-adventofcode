library(stringr)
library(purrr)
input <- readLines("2020/18-input")

`%+%` <- function(a, b) a + b
`%*%` <- function(a, b) a * b

eval_string <- function(x) {
  map_dbl(x, ~eval(parse(text = .x)))
}

options(scipen = 999)
input %>%
  str_replace_all(c("\\+" = "%\\+%", "\\*" = "%\\*%")) %>%
  eval_string() %>%
  sum()
