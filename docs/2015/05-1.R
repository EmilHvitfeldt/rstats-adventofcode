input <- readLines("2015/05-input")

con1 <- stringr::str_count(input, "[aeiou]") >= 3

find_repeat <- function(x) {
  any(rle(x)$lengths > 1)
}

con2 <- vapply(strsplit(input, ""), find_repeat, logical(1))
con3 <- !(grepl("ab", input)) &
  !(grepl("cd", input)) &
  !(grepl("pq", input)) &
  !(grepl("xy", input))

sum(con1 & con2 & con3)
