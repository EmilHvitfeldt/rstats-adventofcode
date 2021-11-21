input <- readLines("2015/03-input")

chars <- strsplit(input, "")[[1]]

path_santa <- data.frame(
  x = cumsum(x_key[chars][seq_along(chars) %% 2 == 1]),
  y = cumsum(y_key[chars][seq_along(chars) %% 2 == 1])
)

path_robosanta <- data.frame(
  x = cumsum(x_key[chars][seq_along(chars) %% 2 == 0]),
  y = cumsum(y_key[chars][seq_along(chars) %% 2 == 0])
)

nrow(unique(rbind(path_santa, path_robosanta)))
