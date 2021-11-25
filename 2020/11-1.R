input <- readLines("2020/11-input")

# turn input into logical matrix
mat <- strsplit(input, "") %>%
  reduce(rbind)

replace <- mat

repeat {
  for (i in seq_len(nrow(mat))) {
    for (j in seq_len(ncol(mat))) {
      if (mat[i, j] == ".") next

      temp_mat <- mat
      temp_mat[i, j] <- ""

      conv <- temp_mat[seq(max(i-1, 1), min(i+1, nrow(mat))),
                       seq(max(j-1, 1), min(j+1, ncol(mat)))]

      if (mat[i, j] == "L") {
        if (all(conv != "#")) {
          replace[i, j] <- "#"
        }
      }

      if (mat[i, j] == "#") {
        if (sum(conv == "#") >= 4) {
          replace[i, j] <- "L"
        }
      }
    }
  }

  if (identical(mat, replace)) break
  mat <- replace
}

sum(mat == "#")
