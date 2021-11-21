input <- readLines("2015/02-input")

chars <- strsplit(input, "x")

ribbon_length <- function(x) {
  x <- as.numeric(x)
  short_sides <- sort(x)[1:2]
  sum(short_sides) * 2 + prod(x)
}

sum(vapply(chars, ribbon_length, numeric(1)))
