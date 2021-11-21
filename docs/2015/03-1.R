input <- readLines("2015/03-input")

chars <- strsplit(input, "")[[1]]

x_key <- c("^" = 0, "v" = 0, ">" = 1, "<" = -1)
y_key <- c("^" = 1, "v" = -1, ">" = 0, "<" = 0)

path <- data.frame(
  x = cumsum(x_key[chars]),
  y = cumsum(y_key[chars])
)

nrow(unique(path))
