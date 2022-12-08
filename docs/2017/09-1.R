input <- readLines("2017/09-input")

library(stringr)

activate_negate <- function(x) {
  before <- x
  repeat {
    after <- str_remove(before, "!.")

    if (before == after) return(before)
    before <- after
  }
}

input <- activate_negate(input)

remove_garbage <- function(x) {
  before <- x
  repeat {
    after <- str_remove(before, "<.*?>")

    if (before == after) return(before)
    before <- after
  }
}

input <- remove_garbage(input)

repeat {
group_loc <- str_locate(input, "\\{[0-9,]*\\}")

group <- str_sub(input, group_loc[, "start"], group_loc[, "end"])

value <- sum(as.numeric(str_extract_all(group, "[0-9]+")[[1]]) + 1) + 1

before <- input
str_sub(input, group_loc[1, "start"], group_loc[1, "end"]) <- value
input
if (is.na(input) || before == input) break
}

before
