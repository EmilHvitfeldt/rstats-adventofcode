input <- readLines("2015/08-input")

sum(purrr::map_int(stringi::stri_escape_unicode(input), nchar) + 2) -
  sum(purrr::map_int(input, nchar))
