library(stringr)

input <- readLines("2018/05-input")

voids <- c(paste0(letters, LETTERS), paste0(LETTERS, letters))

void_regex <- paste0("(", paste(voids, collapse = "|"), ")")

new <- old <- input

repeat {
  new <- str_remove_all(old, void_regex)
  if (new == old) break
  old <- new
}

nchar(new)
