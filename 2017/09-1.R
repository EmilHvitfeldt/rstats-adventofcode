library(stringr)
input <- readLines("2017/09-input")

input <- input |>
  str_remove_all("!.") |>
  str_remove_all("<.*?>")

input <- str_split_1(input, "")

changes <- c("}" = -1, "{" = 1, "," = 0)[input]
levels <- cumsum(changes)
starts <- levels[input == "{"]
sum(starts)
