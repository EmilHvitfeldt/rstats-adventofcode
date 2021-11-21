library(magrittr)
library(stringr)

input <- readLines("2015/07-input")

int_2_16 <- function(x) {
  as.logical(intToBits(x)[1:16])
}

int_2_16_rev <- function(x) {
  sum(2 ^ (0:15) * x)
}

`%AND%` <- function(x, y) {
  int_2_16_rev(int_2_16(x) & int_2_16(y))
}

`%OR%` <- function(x, y) {
  int_2_16_rev(int_2_16(x) | int_2_16(y))
}

`%LSHIFT%` <- function(x, y) {
  int_2_16_rev(c(rep(FALSE, y), int_2_16(x)[seq(1, 16 - y)]))
}

`%RSHIFT%` <- function(x, y) {
  int_2_16_rev(c(int_2_16(x)[seq(y + 1, 16)], rep(FALSE, y)))
}

`%NOT%` <- function(temp, x) {
  int_2_16_rev(!int_2_16(x))
}

eval_fun <- function(x) {
  as.character(eval(parse(text = x)))
}

instructions <- strsplit(input, " -> ")

lhs <- purrr::map_chr(instructions, ~.x[1]) %>%
  str_replace_all(
    c(
      "OR" = "%OR%",
      "AND" = "%AND%",
      "RSHIFT" = "%RSHIFT%",
      "LSHIFT" = "%LSHIFT%",
      "NOT" = "1 %NOT%"
    )
  ) %>%
  paste0("( ", ., " )")

lhs[which(str_detect(lhs, "^\\( [0-9]* \\)$"))] <- str_extract(
  lhs[which(str_detect(lhs, "^\\( [0-9]* \\)$"))],
  "[0-9]+"
)

rhs <- purrr::map_chr(instructions, ~.x[2])

repeat {
  numbers_ind <- which(str_detect(lhs, "^[0-9]*$"))

  if (length(numbers_ind) == length(lhs)) break

  replacement <- str_extract(lhs[numbers_ind], "[0-9]+")
  names(replacement) <- paste0(" ", rhs[numbers_ind], " ")

  lhs <- lhs %>%
    str_replace_all(replacement)

  can_evaluate <- !lhs %>% str_detect("[a-z]+")

  lhs[can_evaluate] <- purrr::map_chr(lhs[can_evaluate], eval_fun)
}

lhs[rhs == "a"]
