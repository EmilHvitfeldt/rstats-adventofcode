input <- readr::read_delim("2016/20-input", delim = "-", col_names = c("min", "max"))

input <- dplyr::arrange(input, min)

used <- 0
count <- 0

for (i in seq_len(nrow(input))) {
  if (input[i, ]$min > used + 1) {
    count <- count + input[i, ]$min - used - 1
  }
  used <- max(input[i, ]$max, used)
}
count
