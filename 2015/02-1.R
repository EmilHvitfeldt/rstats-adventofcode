input <- readLines("2015/02-input")

chars <- strsplit(input, "x")

package_surface <- function(x) {
  x <- as.numeric(x)
  side <- x[1] * x[2]
  front <- x[1] * x[3]
  top <- x[2] * x[3]

  sum(2 * c(side, front, top), min(side, front, top))
}

sum(vapply(chars, package_surface, numeric(1)))
