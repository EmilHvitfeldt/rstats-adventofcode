input <- readLines("2020/13-input")

target <- as.integer(input[1])
ids <- as.integer(str_extract_all(input[2], "[0-9]+")[[1]])

time_past <- ceiling(target / ids) * ids
which_min <- min(time_past) == time_past

(time_past[which_min] - target) * ids[which_min]
