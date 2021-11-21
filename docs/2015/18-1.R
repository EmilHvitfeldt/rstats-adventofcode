input <- readLines("2015/18-input")

library(magrittr)

mat <- strsplit(input, "") %>%
  purrr::map(~.x == "#") %>%
  Reduce(rbind, .)

rownames(mat) <- NULL

mat_update <- matrix(NA, nrow = nrow(mat), ncol = ncol(mat))

max_row <- nrow(mat)
max_col <- ncol(mat)

for (i in seq_len(100)) {
  for (row in seq_len(nrow(mat_update))) {
    for (col in seq_len(ncol(mat_update))) {

      keep <- mat[row, col]

      mat[row, col] <- NA

      neighbors <- sum(na.rm = TRUE,
                       mat[
                         seq(max(row - 1, 1), min(row + 1, max_row)),
                         seq(max(col - 1, 1), min(col + 1, max_col))
                       ]
      )

      if (keep) {
        mat_update[row, col] <- neighbors %in% c(2, 3)
      } else {
        mat_update[row, col] <- neighbors == 3
      }

      mat[row, col] <- keep

    }
  }

  mat <- mat_update
}

sum(mat)
