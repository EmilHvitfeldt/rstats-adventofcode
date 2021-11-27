input <- readLines("2018/02-input")

dist <- stringdist::stringdistmatrix(input, input)

matched_strings <- input[which(dist == 1) %% length(input)]

matched_letters <- strsplit(matched_strings, "")

paste(matched_letters[[1]][matched_letters[[1]] == matched_letters[[2]]], collapse = "")
