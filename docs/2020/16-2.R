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

valid_ticket <- tickets[rowSums(!is.na(wrong)) == 0, ]

can_be <- function(x) {
  which(map_lgl(valid_values, ~all(x %in% .x)))
}

pos <- numeric(length(valid_values))

candidates <- map(valid_ticket, can_be)

repeat {
  variable_ind <- which(lengths(candidates) == 1)
  if(length(variable_ind) == 0) break

  ref_ind <- candidates[[variable_ind]]

  pos[variable_ind] <- ref_ind

  candidates <- map(candidates, setdiff, ref_ind)
}

my_ticket <- as.numeric(str_split(input[min(breaks) + 2], ",")[[1]])

fields <- input[seq(1, min(breaks) - 1)] %>%
  str_detect("^departure")

options(scipen = 999)
prod(my_ticket[fields[pos]])
