input <- readLines("2023/01-input")

numbers <- c(
  "one" = 1, "two" = 2, "three" = 3, "four" = 4, "five" = 5,
  "six" = 6, "seven" = 7, "eight" = 8, "nine" = 9,
  setNames(nm = 1:9)
)
srebmun <- numbers
names(srebmun) <- stringi::stri_reverse(names(srebmun))

get_digit <- function(x, ref) {
  res <- x |>
    stringr::str_extract(paste0("(", paste0(names(ref), collapse = "|"), ")"))

  ref[res]
}

sum(
  get_digit(input, numbers) * 10 +
  get_digit(stringi::stri_reverse(input), srebmun)
)
