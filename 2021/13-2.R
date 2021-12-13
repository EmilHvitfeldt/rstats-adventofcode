library(tidyverse)

input <- readLines("2021/13-input")

mid <- which(input == "")

p1 <- function(x) x + 1L

folds <- input[seq(mid + 1, length(input))]
points <- input[seq_len(mid - 1)] %>%
  str_split(",") %>%
  map(as.integer) %>%
  map(p1)


mat <- matrix(
  FALSE,
  nrow = points %>% map_int(~.x[2]) %>% max(),
  ncol = points %>% map_int(~.x[1]) %>% max()
)

for (point in points) {
  mat[point[[2]], point[[1]]] <- TRUE
}

for (fold in folds) {
  axis <- str_extract(fold, "[xy]")
  amount <- str_extract(fold, "[0-9]+") %>% as.integer() %>% p1()

  if (axis == "y") {
    folded <- seq(nrow(mat), amount + 1)
    landed <- seq(amount - length(folded), amount - 1)
    if (min(landed) == 1) {
      mat <- mat[landed, ] | mat[folded, ]
    } else {
      leftover_mat <- mat[seq_len(min(landed) - 1), ]
      mat <- rbind(leftover_mat, mat[landed, ] | mat[folded, ])
    }
  } else {
    folded <- seq(ncol(mat), amount + 1)
    landed <- seq(amount - length(folded), amount - 1)
    if (min(landed) == 1) {
      mat <- mat[, landed] | mat[, folded]
    } else {
      leftover_mat <- mat[, seq_len(min(landed) - 1)]
      mat <- rbind(leftover_mat, mat[, landed] | mat[, folded])
    }
  }
}

reshape2::melt(mat) %>%
  ggplot(aes(Var2, -Var1, fill = value)) +
  geom_raster()
