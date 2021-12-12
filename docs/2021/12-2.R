library(tidyverse)

input <- tibble(input = readLines("2021/12-input")) %>%
  separate(input, into = c("from", "to"))

input <- bind_rows(
  input,
  input %>% mutate(tmp = from, from = to, to = tmp) %>% select(-tmp)
)

adjacency <- split(input$from, input$to)

is_lower <- function(x) tolower(x) == x

paths <- function(current, seen, duplicate) {
  if (current == "end") {
    return(1)
  }
  if (current == "start" & !is.null(seen)) {
    return(0)
  }
  if (is_lower(current) & current %in% seen) {
    if (is.null(duplicate)) {
      duplicate <- current
    } else {
      return(0)
    }
  }
  seen <- c(seen, current)
  out <- 0
  for (i in adjacency[[current]]) {
    out <- out + paths(i, seen, duplicate)
  }
  out
}

paths(current = "start", seen = NULL, duplicate = NULL)
