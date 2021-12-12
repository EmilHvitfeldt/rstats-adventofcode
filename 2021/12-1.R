library(tidyverse)

input <- tibble(input = readLines("2021/12-input")) %>%
  separate(input, into = c("from", "to"))

input <- bind_rows(
  input,
  input %>% mutate(tmp = from, from = to, to = tmp) %>% select(-tmp)
)

last <- function(x) x[length(x)]

add_next_step <- function(x) {
  last_x <- last(x)
  if (last_x == "end") return(list(x))
  new_steps <- input$to[input$from == last_x]

  map(new_steps, ~c(x, .x))
}

is_correct <- function(x) {
  if (length(x) > 1 & last(x) == "start") return(FALSE)
  x <- x[!x %in% c("end", "start")]
  tab <- table(x)
  tab_names <- names(tab)
  small <- str_to_lower(tab_names) == tab_names
  all(tab[small] == 1)
}

validate_path <- function(x) {
  keep(x, is_correct)
}

grow <- function(x) {
  x %>%
    map(add_next_step) %>%
    flatten() %>%
    validate_path()
}

old <- list()
old[[1]] <- "start"

repeat {
  new <- grow(old)

  if (identical(old, new)) break
  old <- new
}

length(old)
