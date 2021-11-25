input <- readLines("2020/13-input")

ids <- as.numeric(str_split(input[2], ",")[[1]])
offset <- seq_along(ids) - 1
offset <- offset[!is.na(ids)]
ids <- ids[!is.na(ids)]

timestamp <- 1
step <- 1

for (i in seq_along(ids)) {
  repeat {
    if ((timestamp + offset[i]) %% ids[i] == 0) break
    timestamp <- timestamp + step
  }

  step <- prod(ids[seq_len(i)])
}

options(scipen = 999)
timestamp
