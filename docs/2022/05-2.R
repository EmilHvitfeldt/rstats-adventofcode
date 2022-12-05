library(tidyverse)

input <- read_lines("2022/05-input")
delim <- which(input == "")

cargo <- input[seq(1, delim - 2)]
insts <- input[seq(delim + 1, length(input))] |>
  str_extract_all("[0-9]+") |>
  map(as.integer)

box_loc <- seq(1, (max(nchar(cargo)) + 1) / 4) * 4 - 2

stacks <- map(box_loc, ~ str_sub(cargo, .x, .x)) |>
  map(setdiff, c("", " ")) |>
  map(rev)

for (inst in insts) {
  from <- inst[2]
  to <- inst[3]
  amount <- inst[1]
  stacks[[to]] <- c(stacks[[to]], tail(stacks[[from]], amount))
  stacks[[from]] <- head(stacks[[from]], -amount)
}

purrr::map_chr(stacks, tail, 1) |> paste(collapse = "")
