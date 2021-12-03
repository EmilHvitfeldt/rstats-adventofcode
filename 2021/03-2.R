input <- readLines("2021/03-input") |>
  strsplit("") |>
  purrr::reduce(rbind)

common_finder <- function(x, fun, even) {
  tab <- table(x)
  if (length(tab) == 2 & tab[1] == tab[2]) return(even)
  names(which(fun(tab) == tab))
}

rate_calculator <- function(mat, fun, even) {
  considered <- !logical(nrow(mat))

  res <- c()

  for (i in seq_len(ncol(mat))) {
    top <- common_finder(mat[considered, i], fun, even)
    res <- c(res, top)
    considered <- considered & (mat[, i] == top)

    if (sum(considered) == 1) break
  }

  mat[considered, ] |>
    paste0(collapse = "") |>
    strtoi(base = 2)
}

oxygen <- rate_calculator(input, max, "1")
co2 <- rate_calculator(input, min, "0")

oxygen * co2
