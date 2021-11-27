library(stringr)
library(magrittr)

input <- readLines("2016/07-input")

TLS_check <- function(x) {
  outside <- str_replace_all(x, "\\[.+?\\]", " ") %>%
    str_detect("([a-z])((?!\\1)[a-z])\\2\\1")

  inside <- str_extract_all(x, "\\[.+?\\]")[[1]] %>%
    paste(collapse = " ") %>%
    str_detect("([a-z])((?!\\1)[a-z])\\2\\1")


  outside & !inside
}

sum(purrr::map_lgl(input, TLS_check))
