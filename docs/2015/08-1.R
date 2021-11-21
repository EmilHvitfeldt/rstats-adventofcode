input <- readLines("2015/08-input")

sum(purrr::map_int(input, nchar)) -
  sum(purrr::map_int(input, ~nchar(eval(parse(text = .x)), type = "bytes")))
