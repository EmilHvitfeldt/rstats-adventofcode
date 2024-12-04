read_matrix <- function(path, sep = "", fill = NA, type = identity) {
  lines <- readLines(path)
  tokens <- strsplit(lines, sep)
  token_lengths <- lengths(tokens)
  res <- matrix(fill, nrow = length(lines), ncol = max(token_lengths))

  for (i in seq_along(lines)) {
    res[i, seq_len(token_lengths[i])] <- type(tokens[[i]])
  }
  res
}

input <- read_matrix("2024/04-input")

a_loc <- reshape2::melt(input == "A") |>
  dplyr::filter(value)

count <- 0

for (i in seq_len(nrow(a_loc))) {
  vals <- a_loc[i, ]
  x <- vals$Var1
  y <- vals$Var2

  if (x == 1 || x == nrow(input) || y == 1 || y == nrow(input)) {
    next
  }
  
  x_values <- paste0(
    input[x + 1, y + 1], input[x + 1, y - 1], 
    input[x - 1, y - 1], input[x - 1, y + 1],
    collapse = ""
  )
  
  valid <- c("MMSS", "MSSM", "SSMM", "SMMS")


  if (x_values %in% valid) {
    count <- count + 1
  }
}

count
