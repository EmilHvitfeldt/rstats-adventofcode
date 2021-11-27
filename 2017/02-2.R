input <- readr::read_tsv("2017/02-input", col_names = FALSE)

evenly <- function(x) {
  outers <- outer(x, x, "/")

  outers[((outers %% 1) == 0) & (outers != 1)]
}

sum(apply(as.matrix(input), 1, evenly))

x <- as.numeric(input[1, ])
