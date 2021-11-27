input <- readLines("2016/06-input")

mat <- unname(Reduce(rbind, strsplit(input, "")))

paste(apply(mat, 2, function(x) names(sort(table(x), TRUE))[1]), collapse = "")
