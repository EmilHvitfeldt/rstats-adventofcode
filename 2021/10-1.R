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

c(")"= 3, "]" = 57, "}" = 1197, ">" = 25137)[corrupt] |>
  sum(na.rm = TRUE)
