library(purrr)
library(stringr)
library(magrittr)

input <- readLines("2016/07-input")

str_chars <- function(x) {
  str_split(x, "")[[1]]
}

get_anagram <- function(x, is_rev = FALSE) {
  inds <- dplyr::lead(x, 2) == x
  mids <- dplyr::lead(x, 1) != x
  first <- x %in% letters
  second <- dplyr::lead(x, 1) %in% letters

  res <- map(which(inds & mids & first & second), ~x[.x + 0:1])
  if(is_rev) {
    res <- map(res, rev)
  }

  res
}

SSL_check <- function(x) {
  outside <- str_replace_all(x, "\\[.+?\\]", " ") %>%
    str_chars() %>%
    get_anagram()

  inside <- str_extract_all(x, "\\[.+?\\]") %>%
    .[[1]] %>%
    paste(collapse = "") %>%
    str_chars() %>%
    get_anagram(is_rev = TRUE)

  any(inside %in% outside)
}

sum(purrr::map_lgl(input, SSL_check))
