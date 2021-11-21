input <- readLines("2015/17-input")

input <- as.numeric(input)

all_sets <- expand.grid(purrr::map(seq_along(input), ~c(F, T)))

matrix_vals <- t(t(as.matrix(all_sets)) * input)

num_containers <- rowSums(matrix_vals[rowSums(matrix_vals) == 150, ] > 0)

table(num_containers)[1]
