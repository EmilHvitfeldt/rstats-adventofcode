input <- readLines("2015/18-input")
library(magrittr)

mat <- strsplit(input, "") %>%
  purrr::map(~.x == "#") %>%
  Reduce(rbind, .)

rownames(mat) <- NULL

mat_update <- matrix(TRUE, nrow = nrow(mat), ncol = ncol(mat))

max_row <- nrow(mat)
max_col <- ncol(mat)

mat[1, 1] <- TRUE
mat[1, max_col] <- TRUE
mat[max_row, 1] <- TRUE
mat[max_row, max_col] <- TRUE

for (i in seq_len(100)) {
  for (row in seq_len(nrow(mat_update))) {
    for (col in seq_len(ncol(mat_update))) {

      if ((row == 1 & col == 1) |
          (row == 1 & col == max_col) |
          (row == max_row & col == 1) |
          (row == max_row & col == max_col)) next

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
