input <- readLines("2015/12_input")
numbers <- stringr::str_extract_all(input, pattern = "-{0,1}[0-9]+")[[1]]

sum(as.numeric(numbers))
