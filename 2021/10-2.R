library(stringr)
library(purrr)

input <- readLines("2021/10-input")

full_pair <- c("\\(\\)" = "", "\\[\\]" = "", "\\{\\}" = "", "<>" = "")

remove_all_pairs <- function(x) {
  old <- x
  repeat {
    new <- str_replace_all(old, full_pair)
    if (old == new) break
    old <- new
  }
  old
}

pair_side <- c(
  "\\(" = "L", "\\[" = "L", "\\{" = "L", "<" = "L",
  "\\)" = "R", "\\]" = "R", "\\}" = "R", ">" = "R"
)

find_corrupted_pair <- function(x) {
  value <- str_replace_all(x, pair_side)
  loc <- str_locate(value, "LR")
  if (is.na(loc[2])) return(NA)
  str_sub(x, loc[2], loc[2])
}

cleaned_errors <- map_chr(input, remove_all_pairs)
corrupt <- map_chr(cleaned_errors, find_corrupted_pair)

incomplete <- cleaned_errors[is.na(corrupt)]

complete_error <- function(x) {
  score <- 0
  repeat {
    last <- str_sub(x, -1, -1)
    pat_com <- c("(" = ")", "[" = "]", "{" = "}", "<" = ">")
    score <- score * 5 + match(last, names(pat_com))
    x <- paste0(x, pat_com[last])
    x <- remove_all_pairs(x)
    if (x == "") break
  }
  score
}

incomplete %>%
  map_dbl(complete_error) %>%
  median()
