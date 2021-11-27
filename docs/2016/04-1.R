input <- readLines("2016/04-input")

decoy_value <- function(x) {
  name <- stringr::str_extract(x, "[a-z\\-]+")
  name <- stringr::str_extract_all(name, "[a-z]")[[1]]

  checksum <- stringr::str_extract(x, "\\[.+")
  checksum <- stringr::str_extract(checksum, "[a-z]+")

  sector_id <- as.numeric(stringr::str_extract(x, "[0-9]+"))

  is_real <- paste(names(sort(table(name), decreasing = TRUE)[1:5]), collapse = "") == checksum

  sector_id * is_real
}

sum(purrr::map_dbl(input, decoy_value))
