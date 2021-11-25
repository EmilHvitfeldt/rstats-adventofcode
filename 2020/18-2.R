library(stringr)
library(purrr)
input <- readLines("2020/18-input")

eval_string_rev <- function(x) {
  rev_env <- new.env()
  rev_env$`+` <- function(a, b) base::`*`(a, b)
  rev_env$`*` <- function(a, b) base::`+`(a, b)
  map_dbl(x, ~eval(parse(text = .x), envir = rev_env))
}

options(scipen = 999)
input %>%
  str_replace_all(c("\\+" = "temp", "\\*" = "+", "temp" = "*")) %>%
  eval_string_rev() %>%
  sum()
