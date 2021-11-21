input <- readLines("2015/05-input")

pairs <- tokenizers::tokenize_character_shingles(input, n = 2)

x <- pairs[[1]]

con1_fun <- function(x) {
  pairs <- names(which(table(x)>1))

  if (length(pairs) == 0) return(FALSE)

  for (i in seq_along(pairs)) {
    if (any(diff(which(pairs[i] == x)) > 1)) return(TRUE)
  }

  FALSE
}

con1 <- vapply(pairs, con1_fun, FUN.VALUE = logical(1))

triplets <- tokenizers::tokenize_character_shingles(input, n = 3)

con2_fun <- function(x) {
  any(substr(x, 1, 1) == substr(x, 3, 3))
}

con2 <- vapply(triplets, con2_fun, FUN.VALUE = logical(1))

sum(con1 & con2)
