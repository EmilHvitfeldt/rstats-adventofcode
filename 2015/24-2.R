input <- as.numeric(readLines("2015/24-input"))

total <- sum(input) / 4

split_vec <- function(input, total) {
  res <- list()
  for (i in seq_along(input)) {
    combs <- combn(input, i)
    colsum_combs <- colSums(combs)
    if (any(colsum_combs == total)) {
      res <- c(res, list(combs[, which(colsum_combs == total)]))
      break
    }
  }
  res
}

first_group <- split_vec(input, total)

split_vec(setdiff(input, first_group[[1]][, 1]), total)

order(apply(first_group[[1]], 2, prod))

prod(first_group[[1]][, 1])
