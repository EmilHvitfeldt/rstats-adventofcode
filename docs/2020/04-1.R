library(purrr)
library(stringr)
library(magrittr)

input <- readLines("2020/04-input")

passports <- str_split(paste(input, collapse = "\n"), "\n\n")[[1]]

matches <- c("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

valid <- str_extract_all(passports, paste0(matches, collapse = "|")) %>%
  map_lgl(~all(matches %in% .x))

sum(valid)
