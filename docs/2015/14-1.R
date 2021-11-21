input <- readLines("2015/14-input")

library(stringr)
library(purrr)

flying_distance <- function(speed, speed_dur, rest_dur, time) {
  sum(rep(c(rep(speed, speed_dur), rep(0, rest_dur)), length.out = time))
}

numbers <- str_extract_all(input, "[0-9]+")
numbers <- map(numbers, as.numeric)
names(numbers) <- str_extract(input, "\\w*")

ditances <- map_dbl(numbers, ~ flying_distance(.x[1], .x[2], .x[3], 2503))
sort(ditances, decreasing = TRUE)[1]
