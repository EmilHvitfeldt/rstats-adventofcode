library(purrr)
library(stringr)
library(magrittr)

input <- readLines("2020/04-input")

passports <- str_split(paste(input, collapse = "\n"), "\n\n")[[1]]

matches <- c("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

valid <- str_extract_all(passports, paste0(matches, collapse = "|")) %>%
  map_lgl(~all(matches %in% .x))

create_list <- function(x) {
  x <- str_split(x, "[ \n]")[[1]]
  x_split <- str_split(x, ":")

  setNames(map(x_split, 2), map(x_split, 1))
}

password_df <- passports[valid] %>%
  map_dfr(create_list)

int <- as.integer

check_byr <- function(x) int(x) >= 1920 & int(x) <= 2002

check_iyr <- function(x) int(x) >= 2010 & int(x) <= 2020

check_eyr <- function(x) int(x) >= 2020 & int(x) <= 2030

check_hgt <- function(x) {
  value <- as.numeric(str_extract(x, "[0-9]+"))
  unit <- str_remove(x, "[0-9]+")

  if_else(
    unit %in% c("in", "cm"),
    if_else(
      unit == "in",
      value >= 59 & value <= 76,
      value >= 150 & value <= 193
    ),
    FALSE
  )
}

check_hcl <- function(x) {
  str_detect(x, "#[0-9a-f]{6}")
}

check_ecl <- function(x) {
  x %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
}

check_pid <- function(x) {
  str_detect(x, "^[0-9]{9}$")
}

library(dplyr)

password_df %>%
  filter(
    check_hgt(hgt),
    check_byr(byr),
    check_hcl(hcl),
    check_ecl(ecl),
    check_pid(pid),
    check_iyr(iyr),
    check_eyr(eyr)
  ) %>%
  nrow()
