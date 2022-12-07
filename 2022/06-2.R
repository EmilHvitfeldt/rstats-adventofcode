input <- readLines("2022/06-input")
input <- strsplit(input, "")[[1]]

tar_length <- 14
offset <- tar_length - 1

for (i in seq_len(length(input) - offset)) {
  if (length(unique(input[seq(i, i + offset)])) == tar_length) {
    break
  }
}
i + offset
