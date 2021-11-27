input <- readr::read_tsv("2017/02-input", col_names = FALSE)

sum(apply(as.matrix(input), 1, function(x) max(x)-min(x)))
