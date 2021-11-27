library(stringr)

input <- readLines("2018/05-input")

voids <- c(paste0(letters, LETTERS), paste0(LETTERS, letters))

void_regex <- paste0("(", paste(voids, collapse = "|"), ")")

reduce_poly <- function(x, string) {
  string <- str_remove_all(string, x)
  new <- old <- string
  repeat {
    new <- str_remove_all(old, void_regex)
    if (new == old) break
    old <- new
  }

  nchar(new)
}

purrr::map_int(paste0("(", letters, "|", LETTERS, ")"), reduce_poly, input) |>
  min()
