library(tidyverse)
input <- readLines("2020/16-input")
breaks <- which(input == "")

valid_values <- input[seq(1, min(breaks) - 1)] %>%
  str_extract_all("[0-9]+") %>%
  map(as.numeric) %>%
  map(~ c(seq(.x[1], .x[2]), seq(.x[3], .x[4])))

tickets <- read.csv("2020/16-input", header = FALSE,
                    skip = which(input == "nearby tickets:"))

all_valid_values <- unlist(valid_values)

check_value <- function(x, ref) {
  map_dbl(x, ~ ifelse(.x %in% ref, NA, .x))
}

wrong <- apply(tickets, 2, check_value, all_valid_values)

sum(wrong, na.rm = TRUE)
